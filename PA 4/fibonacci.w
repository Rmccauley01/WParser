var iterator = 0;
var n = 8;
var x = 2;

var nMinusTwo = 0;
var nMinusOne = 1;
var temp = 0;

while (x < n) {

    temp = nMinusOne + nMinusTwo;
    nMinusTwo = nMinusOne;
    nMinusOne = temp;
    x = x + 1;

}

print "result is ";
print temp;
print "\n";

// NOTE: I am assuming that we start the fibonacci
// sequence at 0. So, the 1st number is 0, the 2nd
// number is 1, and so on...
// 0  -> 1st
// 1  -> 2nd
// 1  -> 3rd
// 2  -> 4th
// 3  -> 5th
// 5  -> 6th
// 8  -> 7th
// 13 -> 8th
// ...