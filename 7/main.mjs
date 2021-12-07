#!/usr/bin/env node

// @ts-check

import {readFile} from 'node:fs/promises'

const testInput = (
  await readFile(new URL('input.txt', import.meta.url), 'utf8')
)
  .split(',')
  .map(Number)

const sampleInput = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]

/** @param {(distance: number) => number} calculateFuel */
const solution = calculateFuel => /** @param {number[]} input */ input => {
  const min = Math.min(...input)
  const max = Math.max(...input)
  return Math.min(
    ...Array.from({length: max - min + 1}, (_, i) => {
      const position = min + i
      return input.reduce(
        (acc, x) => acc + calculateFuel(Math.abs(x - position)),
        0
      )
    })
  )
}

// Part 1
console.log('PART 1')
const part1 = solution(x => x)
console.log('sample:', part1(sampleInput))
console.log('test:', part1(testInput))

console.log()

// Part 2
console.log('PART 2')
const part2 = solution(x => (x * (x + 1)) / 2)
console.log('sample:', part2(sampleInput))
console.log('test:', part2(testInput))
