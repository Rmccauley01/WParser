var a = True;
var b = False;

if (a || b) {
    print "Works 1";
} else {
    print "Fails 1";
}

print "\n";

if (a && b) {
    print "Fails 2";
} else {
    print "Works 2";
}

print "\n";

if (a == b) print "Fails 3"; else print "Works 3";
print "\n";

if (a != b) print "Works 4"; else print "Fails 4";
print "\n";

if (1 <= 1) print "Works 5"; else print "Fails 5";
print "\n";

if (1 >= 1) print "Works 6"; else print "Fails 6";
print "\n";

if (1 < 1) print "Fails 7"; else print "Works 7";
print "\n";

if (1 > 1) print "Fails 8"; else print "Works 8";
print "\n";

if ((1 + 1) != 2) {
    print "Fails 9";
} else {
    print "Works 9";
}
print "\n";

if ((1 - 1) == 0) {
    print "Works 10";
} else {
    print "Fails 10";
}
print "\n";

if ((1 / 1) != 1) {
    print "Fails 11";
} else {
    print "Works 11";
}
print "\n";

if ((1 * 1) == 1) {
    print "Works 12";
} else {
    print "Fails 12";
}
print "\n";

if (1 * 1 == 1) {
    print "Works 13";
} else {
    print "Fails 13";
}
print "\n";

if (!False) {
    print "Works 14";
} else {
    print "Fails 14";
}
print "\n";

if (!False && True) {
    print "Works 15";
} else {
    print "Fails 15";
}
print "\n";