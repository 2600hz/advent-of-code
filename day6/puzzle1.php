<?php

$orbitMapInput = explode(PHP_EOL, trim(file_get_contents('input.txt')));

$orbitMap = array_reduce($orbitMapInput, function($ret, $item) {
    $item = explode(')', $item);
    if(empty($ret)) { $ret = []; }
    $ret[$item[1]] = $item[0];
    return $ret;
});

$result = 0;
foreach ($orbitMap as $key => $value) {
    $result += countOrbits($orbitMap, $key);
}
echo $result.PHP_EOL;

function countOrbits($orbitMap, $object) {
    if(isset($orbitMap[$object])) {
        return 1 + countOrbits($orbitMap, $orbitMap[$object]);
    }
    return 0;
}