/*
 * recHanoi.c
 *
 *  Created on: 17.07.2013
 *      Author: Stefan Wissert
 */

extern int nondet(void);

/*
 * This function returns the optimal amount of steps,
 * needed to solve the problem for n-disks
 */
int hanoi(int n) {
    int ret, tmp;
    if (n == 1) {
		/* return 1; */
		ret = 1; goto RET;
	}
	/* return 2 * (hanoi(n-1)) + 1; */
    hanoi(n-1);
    tmp = get_hanoi();
    ret = 2 * tmp + 1;
RET:
    set_hanoi(ret);
}


int main() {
    int n = nondet();
    if (n < 1 || n > 31) {
    	return 0;
    }
    /* int result = hanoi(n); */
    int result;
    hanoi(n);
    result = get_hanoi();
    if (result >= n) {
        return 0;
    } else {
        ERROR:
        goto ERROR;
    }
}


