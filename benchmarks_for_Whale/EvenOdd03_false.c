/*
 * Recursive implementation integer addition.
 * 
 * Author: Matthias Heizmann
 * Date: 2013-07-13
 * 
 */

extern int nondet(void);

int isOdd(int n);
int isEven(int n);

int isOdd(int n) {
    int ret;
    if (n == 0) {
        /* return 0; */
        ret = 0; goto RET_isOdd;
    } else if (n == 1) {
        /* return 1; */
        ret = 1; goto RET_isOdd;
    } else {
        /* return isEven(n - 1); */
        isEven(n - 1);
        ret = get_isEven(); goto RET_isOdd;
    }
RET_isOdd:
    set_isOdd(ret);
}

int isEven(int n) {
    int ret;
    if (n == 0) {
        /* return 1; */
        ret = 1; goto RET_isEven;
    } else if (n == 1) {
        /* return 0; */
        ret = 0; goto RET_isEven;
    } else {
        /* return isOdd(n - 1); */
        isOdd(n - 1);
        ret = get_isOdd(); goto RET_isEven;
    }
RET_isEven:
    set_isEven(ret);
}


int main() {
    int n = nondet();
    /* int result = isEven(n); */
    int result;
    isEven(n);
    result = get_isEven();
    int mod = n % 2;
    if (result < 0 || result == mod) {
        return 0;
    } else {
        ERROR: 
        goto ERROR;
    }
}
