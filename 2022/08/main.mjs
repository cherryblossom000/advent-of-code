#!/usr/bin/env node

// I did this on my phone

import {readFile} from 'node:fs/promises'

const parse = str =>
	str
		.trim()
		.split('\n')
		.map(x => [...x].map(Number))

const f = (x, xs) => xs.every(y => y < x)

const part1 = trees =>
	trees.reduce(
		(acc, row, i) =>
			acc +
			row.reduce(
				(acc2, x, j) =>
					acc2 +
					(f(x, row.slice(0, j)) ||
						f(x, row.slice(j + 1)) ||
						f(
							x,
							trees.slice(0, i).map(r => r[j])
						) ||
						f(
							x,
							trees.slice(i + 1).map(r => r[j])
						)),
				0
			),
		0
	)

const g = (x, xs) => {
	const i = xs.findIndex(y => y >= x)
	return i === -1 ? xs.length : i + 1
}

const part2 = trees =>
	Math.max(
		...trees.flatMap((row, i) =>
			row.map(
				(x, j) =>
					g(x, row.slice(0, j).reverse()) *
					g(x, row.slice(j + 1)) *
					g(
						x,
						trees
							.slice(0, i)
							.map(r => r[j])
							.reverse()
					) *
					g(
						x,
						trees.slice(i + 1).map(r => r[j])
					)
			)
		)
	)

const x1 = parse(await readFile('sample.txt', 'utf8'))
const x2 = parse(await readFile('input.txt', 'utf8'))

console.log(part1(x1))
console.log(part1(x2))

console.log(part2(x1))
console.log(part2(x2))
