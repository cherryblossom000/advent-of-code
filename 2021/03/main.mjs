#!/usr/bin/env node

import {readFile} from 'node:fs/promises'

/** @typedef {readonly string[]} BinaryNumber */
/** @typedef {readonly BinaryNumber[]} Input */

/** @param {Input} input */
const part1 = input => {
  const gamma = Array.from({length: input[0].length}, (_, i) =>
    input.filter(s => s[i] === '0').length > input.length / 2 ? '0' : '1'
  )
  return (
    parseInt(gamma.join(''), 2) *
    parseInt(gamma.map(x => (x === '0' ? '1' : '0')).join(''), 2)
  )
}

/**
 * @template T
 * @param {readonly T[]} xs
 * @param {(x: T) => number} f
 * @returns {[T[], T[]]}
 */
const partition = (xs, f) =>
  xs.reduce(
    ([ts, fs], x) => (f(x) ? [[...ts, x], fs] : [ts, [...fs, x]]),
    [[], []]
  )

/**
 * @param {Input} input
 * @param {(_: [[BinaryNumber[], BinaryNumber[]]) => BinaryNumber[]} getFiltered
 * @param {number} i
 * @returns {number}
 */
const rate = (input, getFiltered, i = 0) => {
  const filtered = getFiltered(partition(input, s => s[i] === '0'))
  return filtered.length === 1
    ? parseInt(filtered[0].join(''), 2)
    : rate(filtered, getFiltered, i + 1)
}

/** @param {Input} input */
const part2 = input =>
  rate(input, ([zeroes, ones]) =>
    zeroes.length > ones.length ? zeroes : ones
  ) *
  rate(input, ([zeroes, ones]) => (zeroes.length > ones.length ? ones : zeroes))

const input = (await readFile(new URL('input.txt', import.meta.url), 'utf8'))
  .split('\n')
  .map(n => [...n])

console.log(part1(input))
console.log(part2(input))
