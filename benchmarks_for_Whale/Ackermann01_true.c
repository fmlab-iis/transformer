/*
 * Implementation the Ackermann function.
 * http://en.wikipedia.org/wiki/Ackermann_function
 * 
 * Author: Matthias Heizmann
 * Date: 2013-07-13
 * 
 */

extern int nondet(void);

int ackermann(int m, int n) {
    int ret, tmp;
    if (m==0) {
        /* return n+1; */
        ret = n+1; goto RET;
    }
    if (n==0) {
        /* return ackermann(m-1,1); */
        ackermann(m-1,1);
        ret = get_ack1(); goto RET;
    }
    /* return ackermann(m-1,ackermann(m,n-1)); */
    ackermann(m, n-1);
    tmp = get_ack1();
    ackermann(m-1,tmp);
    ret = get_ack1(); goto RET;
RET:
    set_ack1(ret);
}


int main() {
    int m = nondet();
    if (m < 0 || m > 3) {
        return 0;
    }
    int n = nondet();
    if (n < 0 || n > 23) {
        return 0;
    }
    /* int result = ackermann(m,n); */
    int result;
    ackermann(m,n);
    result = get_ack1();
    if (m < 0 || n < 0 || result >= 0) {
        return 0;
    } else {
        ERROR: 
        goto ERROR;
    }
}
