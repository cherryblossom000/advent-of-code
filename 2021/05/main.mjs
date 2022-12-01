#!/usr/bin/env node

// @ts-check

import {readFile} from 'node:fs/promises'

/** @typedef {[number, number]} Point */
/** @typedef {[Point, Point]} Line */

/** @param {readonly Line[]} lines */
const solution = lines =>
  lines
    .reduce((field, [[x1, y1], [x2, y2]]) => {
      if (x1 === x2) {
        // Vertical
        const column = (field[x1] ??= [])
        const ya = Math.min(y1, y2)
        const yb = Math.max(y1, y2)
        for (let y = ya; y <= yb; y++) {
          column[y] ??= 0
          column[y]++
        }
      } else {
        const xa = Math.min(x1, x2)
        const xb = Math.max(x1, x2)
        if (y1 === y2) {
          // Horizontal
          for (let x = xa; x <= xb; x++) {
            const column = (field[x] ??= [])
            column[y1] ??= 0
            column[y1]++
          }
        } else {
          // Diagonal
          const [ya, yb] = xa === x1 ? [y1, y2] : [y2, y1]
          const m = yb > ya ? 1 : -1
          for (let x = xa; x <= xb; x++) {
            const i = ya + m * (x - xa)
            const column = (field[x] ??= [])
            column[i] ??= 0
            column[i]++
          }
        }
      }
      return field
    }, /** @type {number[][]} */ ([]))
    .flat()
    .filter(x => x > 1).length

/** @param {readonly Line[]} input */
const part1 = input =>
  solution(input.filter(([[x1, y1], [x2, y2]]) => x1 === x2 || y1 === y2))

const part2 = solution

const input = (await readFile(new URL('input.txt', import.meta.url), 'utf8'))
  .split('\n')
  .map(
    l =>
      /** @type {Line} */ (l.split(' -> ').map(p => p.split(',').map(Number)))
  )

console.log(part1(input))
console.log(part2(input))
