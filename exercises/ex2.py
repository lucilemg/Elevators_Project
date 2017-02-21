# Python 3.3.3 and 2.7.6
# python helloworld_python.py

from threading import Thread
import threading

i = 0

lock = threading.Lock()

def thread1_Function():
	global i
	lock.acquire()
	for x in range(0,1000000):
		i += 1
	lock.release()

def thread2_Function():
	global i
	lock.acquire()
	for x in range(0,1000000):
		i -= 1
	lock.release()


def main():
    global i

    thread1 = Thread(target = thread1_Function, args = (),)
    thread2 = Thread(target = thread2_Function, args = (),)
    
    thread1.start()
    thread2.start()
    
    thread1.join()

    thread2.join()
    print(i)


main()
