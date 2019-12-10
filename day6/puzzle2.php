<?php

$orbitMapInput = explode(PHP_EOL, trim(file_get_contents('input.txt')));

$orbitMap = array_reduce($orbitMapInput, function($ret, $item) {
    $item = explode(')', $item);
    if(empty($ret)) { $ret = []; }
    $ret[$item[1]] = $item[0];
    return $ret;
});

$originParentOrbits = getParentOrbits($orbitMap, 'YOU');
$destinationParentOrbits = getParentOrbits($orbitMap, 'SAN');
$commonOrbits = array_values(array_intersect($originParentOrbits, $destinationParentOrbits));

$result = array_search($commonOrbits[0], $originParentOrbits) + array_search($commonOrbits[0], $destinationParentOrbits);
echo $result.PHP_EOL;

function getParentOrbits($orbitMap, $object) {
    if(isset($orbitMap[$object])) {
        return array_merge([$orbitMap[$object]], getParentOrbits($orbitMap, $orbitMap[$object]));
    }
    return [];
}