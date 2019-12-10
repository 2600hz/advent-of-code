import { pipePromise, pipe, sumUp, head } from '../utils.js';
import { getMapData, getOrbitees } from './helpers.js';

/**
 * --- Part Two ---
 * Now, you just need to figure out how many orbital transfers you (YOU) need to take to get to
 * Santa (SAN).
 *
 * You start at the object YOU are orbiting; your destination is the object SAN is orbiting. An
 * orbital transfer lets you move from any object to an object orbiting or orbited by that object.
 *
 * For example, suppose you have the following map:
 *
 * COM)B
 * B)C
 * C)D
 * D)E
 * E)F
 * B)G
 * G)H
 * D)I
 * E)J
 * J)K
 * K)L
 * K)YOU
 * I)SAN
 *
 * Visually, the above map of orbits looks like this:
 *
 *                           YOU
 *                          /
 *         G - H       J - K - L
 *        /           /
 * COM - B - C - D - E - F
 *                \
 *                 I - SAN
 *
 * In this example, YOU are in orbit around K, and SAN is in orbit around I. To move from K to I, a
 * minimum of 4 orbital transfers are required:
 *
 * K to J
 * J to E
 * E to D
 * D to I
 *
 * Afterward, the map of orbits looks like this:
 *
 *         G - H       J - K - L
 *        /           /
 * COM - B - C - D - E - F
 *                \
 *                 I - SAN
 *                  \
 *                   YOU
 *
 * What is the minimum number of orbital transfers required to move from the object YOU are orbiting
 * to the object SAN is orbiting? (Between the objects they are orbiting - not between YOU and SAN.)
 */

// :: [String] -> (Object -> Object)
const objectsToFindFeeder = objects => map => ({ map, objects });

const feedObjectsToFind = objectsToFindFeeder(['YOU', 'SAN']);

const resolveObjectsOribtees = ({map, objects}) => ({
  map,
  objects: objects.map(object => getOrbitees({ map, object }))
})

// :: Object -> [Number]
const findTransfersCountsBetweenObjects = ({map, objects: [origin, destination]}) =>
  origin.
    filter(object => destination.includes(object)).
    map(object => sumUp(
      origin.slice(origin.indexOf(object), -1).length,
      destination.slice(destination.indexOf(object) + 1).length,
    ));

const resolveMinimumTransfersCount = pipe(
  a => a.sort(),
  head
);

const universalOrbitMap = pipePromise(
  getMapData,
  feedObjectsToFind,
  resolveObjectsOribtees,
  findTransfersCountsBetweenObjects,
  resolveMinimumTransfersCount,
  console.log
);

universalOrbitMap(import.meta.url).catch(console.log);
