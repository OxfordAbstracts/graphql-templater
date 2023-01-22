import {distance, closest} from 'fastest-levenshtein';


export const closestImpl = target => options => closest(target, options);

export const distanceImpl = w1 => w2 => distance(w1, w2);