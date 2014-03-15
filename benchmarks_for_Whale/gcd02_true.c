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
        ret = 0; goto RET_gcd;
    }
    if (y1 == y2) {
        /* return y1; */
        ret = y1; goto RET_gcd;
    }
    if (y1 > y2) {
        /* return gcd(y1 - y2, y2); */
        gcd(y1 - y2, y2);
        ret = get_gcd1(); goto RET_gcd;
    }
    /* return gcd(y1, y2 - y1); */
    gcd(y1, y2 - y1); 
    ret = get_gcd1(); goto RET_gcd;
RET_gcd:
    set_gcd1(ret);
}

// does n divide m?
int divides(int n, int m) {
    int ret;
    if (m == 0) {
        /* return 1; // true */
        ret = 1; goto RET_divides;
    }
    if (n > m) {
        /* return 0; // false */
        ret = 0; goto RET_divides;
    }
    /* return divides(n, m - n); */
    divides(n, m - n);
    ret = get_div1(); goto RET_divides;
RET_divides:
    set_div1(ret);
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
    if (m > 0 && n > 0) {
        /* int z = gcd(m, n); */
        int z;
        gcd(m, n);
        z = get_gcd1();
        int tmp;
        divides(z, m);
        tmp = get_div1();
        if (tmp == 0) {
            ERROR:
            goto ERROR;
        } else {
            return 0;
        }
    }
}
