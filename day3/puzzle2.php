<?php

// Loading the input into an array of strings, using end of lines as a delimiter
$wires = explode(PHP_EOL, trim(file_get_contents('input.txt')));
$wire1 = $wires[0];
$wire2 = $wires[1];

$wire1Locations = listWireLocations($wire1);
$wire2Locations = listWireLocations($wire2);
$commonLocations = array_unique(array_intersect(array_keys($wire1Locations), array_keys($wire2Locations)));

$shortestDistance = INF;
foreach ($commonLocations as $loc) {
    $distance = $wire1Locations[$loc] + $wire2Locations[$loc];
    if($distance < $shortestDistance) {
        $shortestDistance = $distance;
    }
}

echo $shortestDistance.PHP_EOL;

function listWireLocations($wire) {
    $currentLocation = [0, 0];
    $wireLocations = [];
    $wirePaths = explode(',', $wire);
    $walkedSteps = 0;
    foreach ($wirePaths as $path) {
        $direction = substr($path, 0, 1);
        $steps = substr($path, 1);
        switch ($direction) {
            case 'L':
                for ($i=0; $i < $steps; $i++) { 
                    $currentLocation[0]--;
                    $walkedSteps++;
                    $loc = join($currentLocation, ',');
                    if(!isset($wireLocations[$loc])) {
                        $wireLocations[$loc] = $walkedSteps;
                    }
                }
                break;
            case 'R':
                for ($i=0; $i < $steps; $i++) { 
                    $currentLocation[0]++;
                    $walkedSteps++;
                    $loc = join($currentLocation, ',');
                    if(!isset($wireLocations[$loc])) {
                        $wireLocations[$loc] = $walkedSteps;
                    }
                }
                break;
            case 'U':
                for ($i=0; $i < $steps; $i++) { 
                    $currentLocation[1]++;
                    $walkedSteps++;
                    $loc = join($currentLocation, ',');
                    if(!isset($wireLocations[$loc])) {
                        $wireLocations[$loc] = $walkedSteps;
                    }
                }
                break;
            case 'D':
                for ($i=0; $i < $steps; $i++) { 
                    $currentLocation[1]--;
                    $walkedSteps++;
                    $loc = join($currentLocation, ',');
                    if(!isset($wireLocations[$loc])) {
                        $wireLocations[$loc] = $walkedSteps;
                    }
                }
                break;
            default:
                break;
        }
    }

    return $wireLocations;
}