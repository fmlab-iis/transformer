/*
 * Recursive implementation multiplication by repeated addition
 * Check that this multiplication is commutative
 * 
 * Author: Jan Leike
 * Date: 2013-07-17
 * 
 */

extern int nondet(void);

// Multiplies two integers n and m
int mult(int n, int m) {
    int ret, tmp;
    if (m < 0) {
        /* return mult(n, -m); */
        mult(n, -m);
        ret = get_mult(); goto RET;
    }
    if (m == 0) {
        /* return 0; */
        ret = 0; goto RET;
    }
    /* return n + mult(n, m - 1); */
     mult(n, m - 1);
     tmp = get_mult();
     ret = n + tmp; goto RET;
RET:
     set_mult(ret);
}

int main() {
    int m = nondet();
    if (m < 0 || m > 46340) {
        return 0;
    }
    int n = nondet();
    if (n < 0 || n > 46340) {
        return 0;
    }
    /* int res1 = mult(m, n); */
    int res1;
    mult(m, n);
    res1 = get_mult();
    /* int res2 = mult(n, m); */
    int res2;
    mult(n, m);
    res2 = get_mult();
    if (res1 != res2 && m > 0 && n > 0) {
        ERROR:
        goto ERROR;
    } else {
        return 0;
    }
}
