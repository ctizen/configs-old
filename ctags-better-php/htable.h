/*
*
*   Copyright (c) 2014, Red Hat, Inc.
*   Copyright (c) 2014, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   Defines hashtable
*/
#ifndef _HTABLE_H
#define _HTABLE_H

#include "general.h"

typedef struct sHashTable hashTable;
typedef unsigned int (* hashTableHashFunc)  (void * key);
typedef boolean      (* hashTableEqualFunc) (void* a, void* b);
typedef void         (* hashTableFreeFunc)  (void * ptr);
typedef void         (* hashTableForeachFunc) (void *key, void *value, void* user_data);

unsigned int hash_ptrhash (void * x);
boolean hash_ptreq (void *a, void *b);

extern hashTable* hashTableNew         (unsigned int size,
					hashTableHashFunc hashfn,
					hashTableEqualFunc equalfn,
					hashTableFreeFunc keyfreefn,
					hashTableFreeFunc valfreefn);
extern void       hashTableDelete      (hashTable *htable);
extern void       hashTablePutItem     (hashTable *htable, void *key, void *value);
extern void*      hashTableGetItem     (hashTable *htable, void *key);
extern boolean    hashTableHasItem     (hashTable *htable, void *key);
extern boolean    hashTableDeleteItem  (hashTable *htable, void *key);
extern void       hashTableForeachItem (hashTable *htable, hashTableForeachFunc proc, void *user_data);

#endif	/* _HTABLE_H */

/* vi:set tabstop=4 shiftwidth=4: */
