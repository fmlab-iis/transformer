/*
 * Recursive implementation of prime number test
 * (Sieve of Eratosthenes)
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
        ret = get_mult(); goto RET_mult;
    }
    if (m == 0) {
        /* return 0; */
        ret = 0; goto RET_mult;
    }
    /* return n + mult(n, m - 1); */
     mult(n, m - 1);
     tmp = get_mult();
     ret = n + tmp; goto RET_mult;
RET_mult:
     set_mult(ret);
}

// Is n a multiple of m?
int multiple_of(int n, int m) {
    int ret;
    if (m < 0) {
        /* return multiple_of(n, -m); */
        multiple_of(n, -m);
        ret = get_multiple_of(); goto RET_multiple_of;
    }
    if (n < 0) {
        /* return multiple_of(-n, m); // false */
        multiple_of(-n, m);
        ret = get_multiple_of(); goto RET_multiple_of;
    }
    if (m == 0) {
        /* return 0; // false */
        ret = 0; goto RET_multiple_of;
    }
    if (n == 0) {
        /* return 1; // true */
        ret = 1; goto RET_multiple_of;
    }
    /* return multiple_of(n - m, m); */
    multiple_of(n - m, m);
    ret = get_multiple_of(); goto RET_multiple_of;
RET_multiple_of:
    set_multiple_of(ret);
}


int is_prime_(int n, int m);
int is_prime(int n);

// Is n prime?
int is_prime(int n) {
    int ret;
    /* return is_prime_(n, n - 1); */
    is_prime_(n, n - 1);
    ret = get_is_prime_(); goto RET_is_prime;
RET_is_prime:
    set_is_prime(ret);
}


int is_prime_(int n, int m) {
    int ret;
    if (n <= 1) {
        /* return 0; // false */
        ret = 0; goto RET_is_prime_;
    }
    if (n == 2) {
        /* return 1; // true */
        ret = 1; goto RET_is_prime_;
    }
    if (n > 2) {
        if (m <= 1) {
            /* return 1; // true */
            ret = 1; goto RET_is_prime_;
        } else {
            if (multiple_of(n, m) == 0) {
                /* return 0; // false */
                ret = 0; goto RET_is_prime_;
            }
            /* return is_prime_(n, m - 1); */
            is_prime_(n, m - 1);
            ret = get_is_prime_(); goto RET_is_prime_;
        }
    }
RET_is_prime_:
    set_is_prime_(ret);
}

int main() {
    int n = nondet();
    if (n < 1 || n > 46340) {
        return 0;
    }
    int result = is_prime(n);
    int f1 = nondet();
    if (f1 < 1 || f1 > 46340) {
        return 0;
    }
    int f2 = nondet();
    if (f1 < 1 || f1 > 46340) {
        return 0;
    }

    int tmp;
    mult(f1, f2);
    tmp = get_mult();
    if (result == 1 && tmp == n && f1 > 1 && f2 > 1) {
        ERROR:
        goto ERROR;
    } else {
        return 0;
    }
}
