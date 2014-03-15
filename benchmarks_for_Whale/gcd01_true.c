/*
 * Recursive implementation of the greatest common denominator
 * using Euclid's algorithm
 * 
 * Author: Jan Leike
 * Date: 2013-07-17
 * 
 */

extern int nondet(void);

// Compute the greatest common denominator using Euclid's algorithm
int gcd(int y1, int y2) {
    int ret;
    if (y1 <= 0 || y2 <= 0) {
        /* return 0;*/
        ret = 0; goto RET;
    }
    if (y1 == y2) {
        /* return y1; */
        ret = y1; goto RET;
    }
    if (y1 > y2) {
        /* return gcd(y1 - y2, y2); */
        gcd(y1 - y2, y2);
        ret = get_gcd1(); goto RET;
    }
    /* return gcd(y1, y2 - y1); */
    gcd(y1, y2 - y1); 
    ret = get_gcd1(); goto RET;
RET:
    set_gcd1(ret);
}

int main() {
    int m = nondet();
    if (m <= 0 || m > 2147483647) {
        return 0;
    }
    int n = nondet();
    if (n <= 0 || n > 2147483647) {
        return 0;
    }
    /* int z = gcd(m, n); */
    int z;
    gcd(m, n);
    z = get_gcd1();
    if (z < 1 && m > 0 && n > 0) {
        ERROR:
        goto ERROR;
    } else {
        return 0;
    }
}
