#!/usr/bin/env elvish

use path

var input = [(from-lines < (path:dir (src)[name])/input.txt)]

# Part 1
+ (for i [(range 1 (count $input))] {
  if (> $input[$i] $input[(- $i 1)]) {
    put 1
  }
})

# Part 2
+ (for i [(range 0 (- (count $input) 3))] {
  if (> $input[(+ $i 3)] $input[$i]) {
    put 1
  }
})
