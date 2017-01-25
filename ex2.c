// gcc 4.7.2 +
// gcc -std=gnu99 -Wall -g -o helloworld_c helloworld_c.c -lpthread

#include <pthread.h>
#include <stdio.h>

static int i = 0;
pthread_mutex_t lock;


// Note the return type: void*
void* thread1_Function(){
	pthread_mutex_lock(&lock);
    for (int k = 0; k < 1000000; k++){
        i++;
    }
    pthread_mutex_unlock(&lock);
    return NULL;
}

void* thread2_Function(){
    pthread_mutex_lock(&lock);
    for (int k = 0; k < 1000000; k++){
        i--;
    }
    pthread_mutex_unlock(&lock);
    return NULL;
}


int main(){
	pthread_mutex_init(&lock, NULL);

    pthread_t thread1;
    pthread_create(&thread1, NULL, thread1_Function, NULL);
    // Arguments to a thread would be passed here ---------^

    pthread_t thread2;
    pthread_create(&thread2, NULL, thread2_Function, NULL);
    
    pthread_join(thread1, NULL);
    pthread_join(thread2, NULL);
    pthread_mutex_destroy(&lock);

    printf("i: %d \n",i);
    return 0;
    
}
