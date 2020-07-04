"""Flood Puzzle."""
import json

import importlib.resources as pkg_resources

from queue import Queue

from flood_solve import puzzles as package_puzzles
from flood_solve.core.color import Color


class Puzzle:
    """Flood Puzzle."""

    def __init__(self, puzzle_id, moves):
        """
        Construct a Puzzle.

        :param puzzle_id: Puzzle ID.
        :param moves: Maximum number of moves allowed to complete the puzzle.
        :return: New puzzle.
        """
        self._puzzle_id = puzzle_id
        self._moves = moves

        self._regions = set()
        self._uncreated_regions = set()
        self._color_map = dict()
        self._neighbor_map = dict()

    def get_id(self):
        """Get the puzzle ID."""
        return self._puzzle_id

    def get_moves(self):
        """Get the maximum number of moves allowed to complete the puzzle."""
        return self._moves

    def get_colors(self):
        """Get the unique colors in the puzzle."""
        return set(self._color_map.values())

    def get_n_non_singular_colors(self):
        """
        Get the number of non-singular colors.

        This is the number of colors that have more than one region and
        can be used as an early stopping condition for faster solving.
        """
        color_count_map = dict()
        for region_id, color in self._color_map.items():
            if color not in color_count_map:
                color_count_map[color] = 0
            color_count_map[color] += 1
        n_non_singular = 0
        for color, count in color_count_map.items():
            if count > 1:
                n_non_singular += 1
        return n_non_singular

    def get_regions_max_neighbor_ranked(self):
        max_neighbors = dict()
        for region_id in self._regions:
            color_frequencies = dict()
            for neighbor_id in self._neighbor_map[region_id]:
                neighbor_color = self._color_map[neighbor_id]
                if neighbor_color not in color_frequencies:
                    color_frequencies[neighbor_color] = 0
                color_frequencies[neighbor_color] += 1
            max_value = max(color_frequencies.values())
            max_neighbors[region_id] = max_value
        sorted_max_neighbors = sorted(max_neighbors.items(), key=lambda x: -x[1])
        return list(tuple(zip(*sorted_max_neighbors))[0])

    def get_regions_centrality_ranked(self, last_region_constraint=None, power=2):
        """
        Get the list of regions ranked by centrality.

        This heuristic is a measure of a region's centrality
        by aggregating the graph distances to all other regions.

        :param last_region_constraint: Only choose regions near the last move region.
        :param power: The exponent on the distance from the source region.
        :return: List of region IDs in order of most central to least central.
        """
        if last_region_constraint is None:
            constrained_ids = self._regions
        else:
            constrained_ids = set()
            for neighbor_id in self._neighbor_map[last_region_constraint]:
                constrained_ids.add(neighbor_id)
                for neighbor_neighbor_id in self._neighbor_map[neighbor_id]:
                    constrained_ids.add(neighbor_neighbor_id)

        centrality_scores = dict()
        for region_id in constrained_ids:
            centrality_score = 0
            visited_set = set()
            region_queue = Queue()
            region_queue.put((0, region_id))
            while not region_queue.empty():
                distance, current_region_id = region_queue.get()
                if current_region_id not in visited_set:
                    centrality_score += distance ** power
                    visited_set.add(current_region_id)
                    for neighbor_id in self._neighbor_map[current_region_id]:
                        region_queue.put((distance + 1, neighbor_id))
            centrality_scores[region_id] = centrality_score
        sorted_centrality_scores = sorted(centrality_scores.items(), key=lambda x: x[1])
        return list(tuple(zip(*sorted_centrality_scores))[0])

    def get_colors_neighbor_ranked(self, region_id):
        """
        Get the list of colors ranked by color frequency of a region's neighbors.

        This heuristic is a measure of color importance based on how much
        work can be done by contracting with each potential color.

        :param region_id: The region ID to check for color importance.
        :return: List of colors in order of most frequent among neighbors to least frequent.
        """
        color_frequencies = dict()
        for neighbor_id in self._neighbor_map[region_id]:
            neighbor_color = self._color_map[neighbor_id]
            if neighbor_color not in color_frequencies:
                color_frequencies[neighbor_color] = 0
            color_frequencies[neighbor_color] += 1
        if len(color_frequencies) == 0:
            return self.get_colors() - set([self._color_map[region_id]])
        sorted_color_frequencies = sorted(color_frequencies.items(), key=lambda x: -x[1])
        return list(tuple(zip(*sorted_color_frequencies))[0])

    def get_region_color(self, region_id):
        """
        Get the color of a region.

        :param region_id: Region ID.
        :return: Color of the region.
        """
        return self._color_map[region_id]

    def get_regions(self):
        """Get the set of puzzle regions."""
        return self._regions

    def add_region(self, region_id, region_color):
        """
        Add a new region to the puzzle.

        :param region_id: Region ID.
        :param region_color: Region color.
        """
        if region_id in self._regions:
            raise ValueError(f"Region {region_id} already exists in the puzzle")
        if region_id in self._color_map and self._color_map[region_id] != region_color:
            raise ValueError(f"Color {self._color_map[region_id]} already assigned to region {region_id}")

        self._regions.add(region_id)
        self._color_map[region_id] = region_color
        if region_id not in self._neighbor_map:
            self._neighbor_map[region_id] = set()

        if region_id in self._uncreated_regions:
            self._uncreated_regions.remove(region_id)

    def assign_neighbor(self, region_id, neighbor_id):
        """
        Assign one region to be a neighbor of another.

        :param region_id: ID of a region.
        :param neighbor_id: ID of a neighboring region.
        """
        if region_id not in self._regions:
            self._uncreated_regions.add(region_id)
        if neighbor_id not in self._regions:
            self._uncreated_regions.add(neighbor_id)

        if region_id not in self._neighbor_map:
            self._neighbor_map[region_id] = set()
        self._neighbor_map[region_id].add(neighbor_id)

    def contract(self, region_id, color):
        """
        Update a region to a new color.

        - Region A gets color C
        - Neighbors of A that are already color C are removed from the puzzle
        - Region A gains the neighbors of all removed neighbors (except itself)
        """
        if self._color_map[region_id] == color:
            raise ValueError(f"Region {region_id} is already {color}")

        if len(self._uncreated_regions) != 0:
            raise ValueError("Cannot contract puzzle with uncreated regions")

        self._color_map[region_id] = color
        self._moves = self._moves - 1

        neighbor_set = self._neighbor_map[region_id]
        neighbors_to_remove = set()
        neighbors_to_add = set()
        for neighbor_id in neighbor_set:
            if self._color_map[neighbor_id] == color:
                # Update neighbors of same colored neighbors to be neighbors with the target region
                for neighbor_neighbor_id in self._neighbor_map[neighbor_id]:
                    if neighbor_neighbor_id != region_id:
                        self._neighbor_map[neighbor_neighbor_id].remove(neighbor_id)
                        self._neighbor_map[neighbor_neighbor_id].add(region_id)
                        neighbors_to_add.add(neighbor_neighbor_id)
                neighbors_to_remove.add(neighbor_id)

        # Update target region to be neighbors with neighbors of same colored neighbors
        neighbor_set.update(neighbors_to_add)

        # Remove neighbors that are the same color as the target region
        for neighbor_to_remove in neighbors_to_remove:
            neighbor_set.remove(neighbor_to_remove)
            self._regions.remove(neighbor_to_remove)
            del self._color_map[neighbor_to_remove]
            del self._neighbor_map[neighbor_to_remove]

    def _print(self):
        from pprint import pprint
        print("Regions: ")
        pprint(self._regions)
        print("Color Map: ")
        pprint(self._color_map)
        print("Neighbor Map: ")
        pprint(self._neighbor_map)

    def _check_valid(self):
        """Check whether the puzzle is valid."""
        pid = self.get_id()
        if len(self._regions) < 2:
            raise ValueError(f"Not enough regions ({self._regions}) in puzzle {pid}")
        if len(self._uncreated_regions) != 0:
            raise ValueError(f"Uncreated regions {self._uncreated_regions} in {pid}")
            map_set = set(self._color_map.keys())
            raise ValueError(f"Invalid color assignments in {pid}")
        if self._regions != self._neighbor_map.keys():
            raise ValueError(f"Invalid neighbor assignments in {pid}")
        if self._moves < 1:
            raise ValueError(f"Invalid moves: {self._moves}")
        for region_id, neighbor_ids in self._neighbor_map.items():
            if region_id in neighbor_ids:
                raise ValueError(f"Found region {region_id} with self as neighbor")
            for neighbor_id in neighbor_ids:
                if self._color_map[region_id] == self._color_map[neighbor_id]:
                    raise ValueError("Adjacent regions {}/{} share color {}".format(
                        region_id, neighbor_id, self._color_map[region_id]))
                if region_id not in self._neighbor_map[neighbor_id]:
                    raise ValueError(f"Region {region_id} is not a neighbor of its neighbor {neighbor_id}")
        return True
