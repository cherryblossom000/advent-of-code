#!/usr/bin/env elvish

use path

var input = [(from-lines < (path:dir (src)[name])/input.txt)]

+ (for i [(range 1 (count $input))] {
  if (> $input[$i] $input[(- $i 1)]) {
    put 1
  }
})
