#! /bin/sh

<src/main/scala/day12/instructions.txt ruby -rset -e 'turns = Set.new; while gets; if $_.strip =~ /\A[LR]([0-9]+)\Z/; turns << $1.to_i; end; end; puts turns.inspect'