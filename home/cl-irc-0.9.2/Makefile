# $Id: Makefile 2 2004-01-05 14:13:03Z eenge $
# $Source$

clean:
	find -name "*.fasl" -o -name "*.faslmt" -o -name "*~" -o -name "*.err" -o -name "*.x86f" | xargs rm 

commit:
	make clean; cvs up; cvs ci

