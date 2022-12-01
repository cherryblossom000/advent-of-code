#!/usr/bin/env run-cargo-script

use std::{
    cmp::Ordering,
    collections::{BinaryHeap, HashSet},
    fs::File,
    io::{self, BufRead, BufReader},
};

fn parse(path: &str) -> Result<Vec<Vec<u32>>, io::Error> {
    BufReader::new(File::open(path)?)
        .lines()
        .map(|line| {
            Ok(line?
                .chars()
                .map(|c| c.to_digit(10).expect("invalid digit"))
                .collect())
        })
        .collect()
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct Path {
    cost: u32,
    point: (usize, usize),
}

impl PartialOrd for Path {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Path {
    fn cmp(&self, other: &Self) -> Ordering {
        other
            .cost
            .cmp(&self.cost)
            .then_with(|| self.point.cmp(&other.point))
    }
}

fn shortest_path(grid: &[Vec<u32>]) -> u32 {
    let len = grid.len();
    let end = (len - 1, len - 1);

    let mut start = (0, 0);
    let mut heap = BinaryHeap::new();
    let mut visited = HashSet::new();
    let mut cost = 0;

    while start != end {
        let (x1, y1) = start;

        let mut visit = |x2, y2| {
            let p = (x2, y2);
            if !visited.contains(&p) && x2 < len && y2 < len {
                // needs type annotations for some reason
                let row: &Vec<u32> = &grid[y2];
                heap.push(Path {
                    cost: cost + row[x2],
                    point: p,
                });
            }
            let _ = visited.insert(p);
        };
        visit(x1 + 1, y1);
        visit(x1, y1 + 1);
        if x1 > 0 {
            visit(x1 - 1, y1);
        }
        if y1 > 0 {
            visit(x1, y1 - 1);
        }

        let path = heap.pop().expect("no path");
        start = path.point;
        cost = path.cost;
    }
    cost
}

fn part_1(input: &[Vec<u32>]) -> u32 {
    shortest_path(input)
}

fn add_risk(x: u32, y: u32) -> u32 {
    let n = (x + y) % 9;
    if n == 0 {
        9
    } else {
        n
    }
}

fn extend_grid(grid: &[Vec<u32>]) -> Vec<Vec<u32>> {
    let len = grid.len();

    let mut new_grid = grid.to_owned();
    new_grid.resize(5 * len, vec![]);
    for (i, line) in grid.iter().enumerate() {
        let new = (1..=4)
            .flat_map(|x| line.iter().map(move |&n| add_risk(n, x)))
            .collect::<Vec<_>>();

        for j in 1..=4 {
            new_grid[i + j as usize * len]
                .extend(line.iter().chain(new.iter()).map(|&n| add_risk(n, j)));
        }

        new_grid[i].extend(new);
    }
    new_grid
}

fn part_2(input: &[Vec<u32>]) -> u32 {
    shortest_path(&extend_grid(input))
}

fn main() -> Result<(), io::Error> {
    let sample = parse("sample.txt")?;
    let input = parse("input.txt")?;

    println!("{}", part_1(&sample));
    println!("{}", part_1(&input));
    println!();
    println!("{}", part_2(&sample));
    println!("{}", part_2(&input));

    Ok(())
}
