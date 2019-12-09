<?php

// Loading the input into an array of strings, using commas as a delimiter
// Note: no need to cast those strings into ints, PHP will automagically do that when performing operations
$initialIntcode = explode(',', trim(file_get_contents('input.txt')));

runIntcode($initialIntcode);

function runIntcode($intcode) {
    $i = 0;
    while(isset($intcode[$i])) {
        $opcode = substr($intcode[$i], -2);
        $params = strrev(substr($intcode[$i], 0, -2));

        switch ($opcode) {
            case '01':
                $param1 = isset($params[0]) && $params[0] == 1 ? $intcode[$i+1] : $intcode[$intcode[$i+1]];
                $param2 = isset($params[1]) && $params[1] == 1 ? $intcode[$i+2] : $intcode[$intcode[$i+2]];
                $intcode[$intcode[$i+3]] = $param1 + $param2;
                $i += 4;
                break;
            case '02':
                $param1 = isset($params[0]) && $params[0] == 1 ? $intcode[$i+1] : $intcode[$intcode[$i+1]];
                $param2 = isset($params[1]) && $params[1] == 1 ? $intcode[$i+2] : $intcode[$intcode[$i+2]];
                $intcode[$intcode[$i+3]] = $param1 * $param2;
                $i += 4;
                break;
            case '03':
                $prompt = readline('Input: ');
                $intcode[$intcode[$i+1]] = $prompt;
                $i += 2;
                break;
            case '04':
                $param = isset($params[0]) && $params[0] == 1 ? $intcode[$i+1] : $intcode[$intcode[$i+1]];
                echo "Output: $param".PHP_EOL;
                $i += 2;
                break;
            case '05':
                $param1 = isset($params[0]) && $params[0] == 1 ? $intcode[$i+1] : $intcode[$intcode[$i+1]];
                $param2 = isset($params[1]) && $params[1] == 1 ? $intcode[$i+2] : $intcode[$intcode[$i+2]];
                if($param1 != '0') {
                    $i = $param2;
                } else {
                    $i += 3;
                }
                break;
            case '06':
                $param1 = isset($params[0]) && $params[0] == 1 ? $intcode[$i+1] : $intcode[$intcode[$i+1]];
                $param2 = isset($params[1]) && $params[1] == 1 ? $intcode[$i+2] : $intcode[$intcode[$i+2]];
                if($param1 == '0') {
                    $i = $param2;
                } else {
                    $i += 3;
                }
                break;
            case '07':
                $param1 = isset($params[0]) && $params[0] == 1 ? $intcode[$i+1] : $intcode[$intcode[$i+1]];
                $param2 = isset($params[1]) && $params[1] == 1 ? $intcode[$i+2] : $intcode[$intcode[$i+2]];
                $intcode[$intcode[$i+3]] = $param1 < $param2 ? 1 : 0;
                $i += 4;
                break;
            case '08':
                $param1 = isset($params[0]) && $params[0] == 1 ? $intcode[$i+1] : $intcode[$intcode[$i+1]];
                $param2 = isset($params[1]) && $params[1] == 1 ? $intcode[$i+2] : $intcode[$intcode[$i+2]];
                $intcode[$intcode[$i+3]] = $param1 == $param2 ? 1 : 0;
                $i += 4;
                break;
            case '99':
                echo "Opcode 99 encountered. Intcode halted.".PHP_EOL;
                return $intcode[0];
            default:
                echo "Unknown Opcode at position $i. Intcode terminated.".PHP_EOL;
                die;
        }
    }
}