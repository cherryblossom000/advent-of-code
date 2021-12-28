#!/usr/bin/env node

// @ts-check

import {readFile} from 'node:fs/promises'

/** @typedef {readonly [string, number]} Command */

/** @param {Command[]} input */
const part1 = input => {
  const [height, depth] = input.reduce(
    ([h, d], [dir, x]) =>
      dir === 'forward' ? [h + x, d] : dir === 'up' ? [h, d - x] : [h, d + x],
    [0, 0]
  )
  return height * depth
}

/** @param {Command[]} input */
const part2 = input => {
  const [height, depth] = input.reduce(
    ([h, d, a], [dir, x]) =>
      dir === 'forward'
        ? [h + x, d + a * x, a]
        : dir === 'up'
        ? [h, d, a - x]
        : [h, d, a + x],
    [0, 0, 0]
  )
  return height * depth
}

const input = (await readFile(new URL('input.txt', import.meta.url), 'utf8'))
  .split('\n')
  .map(s => {
    const [d, n] = s.split(' ')
    return /** @type {Command} */ ([d, Number(n)])
  })

console.log(part1(input))
console.log(part2(input))
