#!/bin/bash

$test = tests/t1.exp

expect -f $test lc3-soln > soln.result

expect -f $test lc3 > mine.result


vimdiff soln.result mine.result

