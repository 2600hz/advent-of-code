<?php

// Loading the input into an array of strings, using commas as a delimiter
// Note: no need to cast those strings into ints, PHP will automagically do that when performing operations
$initialIntcode = explode(',', trim(file_get_contents('input.txt')));

$targetValue = 19690720;

for ($n=0; $n <= 99; $n++) { 
    for ($v=0; $v <= 99; $v++) { 
        if(runIntcode($initialIntcode, $n, $v) == $targetValue) {
            $result = 100 * $n + $v;
            break 2;
        }
    }
}

if(!empty($result)) {
    echo $result.PHP_EOL;
} else {
    echo 'No result found.'.PHP_EOL;
}

function runIntcode($intcode, $noun, $verb) {
    $intcode[1] = $noun;
    $intcode[2] = $verb;

    for ($i=0; $i < (count($intcode)-4); $i+=4) { 
        if($intcode[$i] == 1) {
            $intcode[$intcode[$i+3]] = $intcode[$intcode[$i+1]] + $intcode[$intcode[$i+2]];
        } elseif($intcode[$i] == 2) {
            $intcode[$intcode[$i+3]] = $intcode[$intcode[$i+1]] * $intcode[$intcode[$i+2]];
        } elseif($intcode[$i] == 99) {
            break;
        } else {
            echo "Unknown Opcode at position $i. Intcode terminated.".PHP_EOL;
            die;
        }
    }

    return $intcode[0];
}