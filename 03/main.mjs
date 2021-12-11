#!/usr/bin/env node

import {readFile} from 'node:fs/promises'

const input = (await readFile(new URL('input.txt', import.meta.url), 'utf8'))
  .split('\n')
  .map(n => [...n])

// Part 1
const gamma = Array.from({length: input[0].length}, (_, i) =>
  input.filter(s => s[i] === '0').length > input.length / 2 ? '0' : '1'
)
console.log(
  parseInt(gamma.join(''), 2) *
    parseInt(gamma.map(x => (x === '0' ? '1' : '0')).join(''), 2)
)

// Part 2
const partition = (xs, f) =>
  xs.reduce(
    ([ts, fs], x) => (f(x) ? [[...ts, x], fs] : [ts, [...fs, x]]),
    [[], []]
  )

const part2O2 = (input, i = 0) => {
  const [zeroes, ones] = partition(input, s => s[i] === '0')
  const filtered = zeroes.length > ones.length ? zeroes : ones
  return filtered.length === 1
    ? parseInt(filtered[0].join(''), 2)
    : part2O2(filtered, i + 1)
}
const part2CO2 = (input, i = 0) => {
  const [zeroes, ones] = partition(input, s => s[i] === '0')
  const filtered = zeroes.length > ones.length ? ones : zeroes
  return filtered.length === 1
    ? parseInt(filtered[0].join(''), 2)
    : part2CO2(filtered, i + 1)
}
console.log(part2O2(input) * part2CO2(input))
