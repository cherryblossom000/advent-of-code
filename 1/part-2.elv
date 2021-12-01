#!/usr/bin/env elvish

use path

var input = [(from-lines < (path:dir (src)[name])/input.txt)]

+ (for i [(range 0 (- (count $input) 3))] {
  if (> $input[(+ $i 3)] $input[$i]) {
    put 1
  }
})
