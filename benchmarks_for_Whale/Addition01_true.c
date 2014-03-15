/*
 * Recursive implementation integer addition.
 * 
 * Author: Matthias Heizmann
 * Date: 2013-07-13
 * 
 */

extern int nondet(void);

int addition(int m, int n) {
    int ret;
    if (n == 0) {
        ret = m; goto RET;
    }
    if (n > 0) {
        /* return addition(m+1, n-1); */
        addition(m+1, n-1);
        ret = get_add1(); goto RET;
    }
    if (n < 0) {
        /* return addition(m-1, n+1); */
        addition(m-1,n+1);
        ret = get_add1(); goto RET;
    }
RET:
    set_add1(ret);
}


int main() {
    int m = nondet();
    if (m < 0 || m > 2147483647) {
        return 0;
    }
    int n = nondet();
    if (n < 0 || n > 2147483647) {
        return 0;
    }
    /* int result = addition(m,n); */
    int result;
    addition(m,n);
    result = get_add1();
    if (result == m + n) {
        return 0;
    } else {
        ERROR: 
        goto ERROR;
    }
}
