<?php

$initialIntcode = explode(',', trim(file_get_contents('input.txt')));

$signalValues = [];
$maxPhases = base_convert(44444, 5, 10);

for ($i=0; $i <= $maxPhases; $i++) {
    $phases = str_pad(base_convert($i, 10, 5), 5, "0", STR_PAD_LEFT);
    $signalValues[] = runAmps($initialIntcode, $phases);
}

$result = max($signalValues);

echo $result.PHP_EOL;

function runAmps($intcode, $phases) {
    $ampA = runIntcode($intcode, [$phases[0], 0]);
    $ampB = runIntcode($intcode, [$phases[1], array_pop($ampA)]);
    $ampC = runIntcode($intcode, [$phases[2], array_pop($ampB)]);
    $ampD = runIntcode($intcode, [$phases[3], array_pop($ampC)]);
    $ampE = runIntcode($intcode, [$phases[4], array_pop($ampD)]);
    return $ampE[0];
}

function runIntcode($intcode, $inputs=[]) {
    $outputs = [];
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
                $input = array_shift($inputs);
                if(is_null($input)) {
                    $input = readline('Input: ');
                }
                $intcode[$intcode[$i+1]] = $input;
                $i += 2;
                break;
            case '04':
                $param = isset($params[0]) && $params[0] == 1 ? $intcode[$i+1] : $intcode[$intcode[$i+1]];
                $outputs[] = $param;
                // echo "Output: $param".PHP_EOL;
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
                // echo "Opcode 99 encountered. Intcode halted.".PHP_EOL;
                return $outputs;
            default:
                echo "Unknown Opcode at position $i. Intcode terminated.".PHP_EOL;
                die;
        }
    }

    return $outputs;
}