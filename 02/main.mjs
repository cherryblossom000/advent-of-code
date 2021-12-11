#!/usr/bin/env node

import {readFile} from 'node:fs/promises'

const input = (await readFile(new URL('input.txt', import.meta.url), 'utf8'))
  .split('\n')
  .map(s => {
    const [d, n] = s.split(' ')
    return [d, Number(n)]
  })

// Part 1
;(() => {
  const [height, depth] = input.reduce(
    ([h, d], [dir, x]) =>
      dir === 'forward' ? [h + x, d] : dir === 'up' ? [h, d - x] : [h, d + x],
    [0, 0]
  )
  console.log(height * depth)
})()

// Part 2
;(() => {
  const [height, depth] = input.reduce(
    ([h, d, a], [dir, x]) =>
      dir === 'forward'
        ? [h + x, d + a * x, a]
        : dir === 'up'
        ? [h, d, a - x]
        : [h, d, a + x],
    [0, 0, 0]
  )
  console.log(height * depth)
})()
