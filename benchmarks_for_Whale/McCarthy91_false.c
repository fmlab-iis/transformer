/*
 * Implementation the McCarthy 91 function.
 * http://en.wikipedia.org/wiki/McCarthy_91_function
 * 
 * Author: Matthias Heizmann
 * Date: 2013-07-13
 * 
 */

extern int nondet(void);


int f91(int x) {
    int ret, tmp;
    if (x > 100) {
        ret = x -10; goto RET;
    } else {
        /* return f91(f91(x+11)); */
        f91(x+11);
        tmp = get_f911();
        f91(tmp);
        ret = get_f911(); goto RET;
    }
RET:
   set_f911(ret);
}


int main() {
    int x = nondet();
    /* int result = f91(x); */
    int result;
    f91(x);
    result = get_f911();
    if (result == 91 || x > 102 && result == x - 10) {
        return 0;
    } else {
        ERROR: 
        goto ERROR;
    }
}
