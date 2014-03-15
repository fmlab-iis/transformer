/*
 * Recursive computation of fibonacci numbers.
 * 
 * Author: Matthias Heizmann
 * Date: 2013-07-13
 * 
 */

extern int nondet(void);
int fibonacci(int n);
int main();


int fibonacci(int n) {
    int ret, tmp1, tmp2;
    if (n < 1) {
        /* return 0; */
        ret = 0; goto RET;
    } else if (n == 1) {
        /* return 1; */
        ret = 1; goto RET;
    } else {
        /* return fibonacci(n-1) + fibonacci(n-2); */
        fibonacci(n-1);
        tmp1 = get_fib1();
        fibonacci(n-2);
        tmp2 = get_fib1();
        ret = tmp1 + tmp2; goto RET;
    }
RET:
    set_fib1(ret);
}


int main() {
    int x = nondet();
    /* int result = fibonacci(x); */
    int result;
    fibonacci(x);
    result = get_fib1();
    if (x != 5 || result == 3) {
        return 0;
    } else {
        ERROR: 
        goto ERROR;
    }
}
    

