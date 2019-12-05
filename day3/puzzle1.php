<?php

// Loading the input into an array of strings, using end of lines as a delimiter
$wires = explode(PHP_EOL, trim(file_get_contents('input.txt')));
$wire1 = $wires[0];
$wire2 = $wires[1];

$wire1Locations = listWireLocations($wire1);
$wire2Locations = listWireLocations($wire2);
$commonLocations = array_unique(array_intersect($wire1Locations, $wire2Locations));

$shortestDistance = INF;
foreach ($commonLocations as $loc) {
    $coords = explode(',', $loc);
    $distance = abs($coords[0]) + abs($coords[1]);
    if($distance < $shortestDistance) {
        $shortestDistance = $distance;
    }
}

echo $shortestDistance.PHP_EOL;

function listWireLocations($wire) {
    $currentLocation = [0, 0];
    $wireLocations = [];
    $wirePaths = explode(',', $wire);
    foreach ($wirePaths as $path) {
        $direction = substr($path, 0, 1);
        $steps = substr($path, 1);
        switch ($direction) {
            case 'L':
                for ($i=0; $i < $steps; $i++) { 
                    $currentLocation[0]--;
                    $wireLocations[] = join($currentLocation, ',');
                }
                break;
            case 'R':
                for ($i=0; $i < $steps; $i++) { 
                    $currentLocation[0]++;
                    $wireLocations[] = join($currentLocation, ',');
                }
                break;
            case 'U':
                for ($i=0; $i < $steps; $i++) { 
                    $currentLocation[1]++;
                    $wireLocations[] = join($currentLocation, ',');
                }
                break;
            case 'D':
                for ($i=0; $i < $steps; $i++) { 
                    $currentLocation[1]--;
                    $wireLocations[] = join($currentLocation, ',');
                }
                break;
            default:
                break;
        }
    }

    return $wireLocations;
}