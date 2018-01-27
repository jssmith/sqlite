/*
** 2008 August 05
**
** The author disclaims copyright to this source code.  In place of
** a legal notice, here is a blessing:
**
**    May you do good and not evil.
**    May you find forgiveness for yourself and forgive others.
**    May you share freely, never taking more than you give.
**
*************************************************************************
** This file implements that page cache.
*/
#include "sqliteInt.h"

/*
** A complete page cache is an instance of this structure.  Every
** entry in the cache holds a single page of the database file.  The
** btree layer only operates on the cached copy of the database pages.
**
** A page cache entry is "clean" if it exactly matches what is currently
** on disk.  A page is "dirty" if it has been modified and needs to be
** persisted to disk.
**
** pDirty, pDirtyTail, pSynced:
**   All dirty pages are linked into the doubly linked list using
**   PgHdr.pDirtyNext and pDirtyPrev. The list is maintained in LRU order
**   such that p was added to the list more recently than p->pDirtyNext.
**   PCache.pDirty points to the first (newest) element in the list and
**   pDirtyTail to the last (oldest).
**
**   The PCache.pSynced variable is used to optimize searching for a dirty
**   page to eject from the cache mid-transaction. It is better to eject
**   a page that does not require a journal sync than one that does. 
**   Therefore, pSynced is maintained to that it *almost* always points
**   to either the oldest page in the pDirty/pDirtyTail list that has a
**   clear PGHDR_NEED_SYNC flag or to a page that is older than this one
**   (so that the right page to eject can be found by following pDirtyPrev
**   pointers).
*/
struct PCache {
  PgHdr *pDirty, *pDirtyTail;         /* List of dirty pages in LRU order */
  PgHdr *pSynced;                     /* Last synced page in dirty page list */
  int nRefSum;                        /* Sum of ref counts over all pages */
  int szCache;                        /* Configured cache size */
  int szSpill;                        /* Size before spilling occurs */
  int szPage;                         /* Size of every page in this cache */
  int szExtra;                        /* Size of extra space for each page */
  u8 bPurgeable;                      /* True if pages are on backing store */
  u8 eCreate;                         /* eCreate value for for xFetch() */
  int (*xStress)(void*,PgHdr*);       /* Call to try make a page clean */
  void *pStress;                      /* Argument to xStress */
  sqlite3_pcache *pCache;             /* Pluggable cache module */
};

/********************************** Test and Debug Logic **********************/
/*
** Debug tracing macros.  Enable by by changing the "0" to "1" and
** recompiling.
**
** When sqlite3PcacheTrace is 1, single line trace messages are issued.
** When sqlite3PcacheTrace is 2, a dump of the pcache showing all cache entries
** is displayed for many operations, resulting in a lot of output.
*/
#if defined(SQLITE_DEBUG) && 1

typedef unsigned long DWORD;
typedef DWORD UNS_32_BITS;
typedef unsigned char BYTE;

static UNS_32_BITS crc_32_tab[] = { /* CRC polynomial 0xedb88320 */
0x00000000, 0x77073096, 0xee0e612c, 0x990951ba, 0x076dc419, 0x706af48f,
0xe963a535, 0x9e6495a3, 0x0edb8832, 0x79dcb8a4, 0xe0d5e91e, 0x97d2d988,
0x09b64c2b, 0x7eb17cbd, 0xe7b82d07, 0x90bf1d91, 0x1db71064, 0x6ab020f2,
0xf3b97148, 0x84be41de, 0x1adad47d, 0x6ddde4eb, 0xf4d4b551, 0x83d385c7,
0x136c9856, 0x646ba8c0, 0xfd62f97a, 0x8a65c9ec, 0x14015c4f, 0x63066cd9,
0xfa0f3d63, 0x8d080df5, 0x3b6e20c8, 0x4c69105e, 0xd56041e4, 0xa2677172,
0x3c03e4d1, 0x4b04d447, 0xd20d85fd, 0xa50ab56b, 0x35b5a8fa, 0x42b2986c,
0xdbbbc9d6, 0xacbcf940, 0x32d86ce3, 0x45df5c75, 0xdcd60dcf, 0xabd13d59,
0x26d930ac, 0x51de003a, 0xc8d75180, 0xbfd06116, 0x21b4f4b5, 0x56b3c423,
0xcfba9599, 0xb8bda50f, 0x2802b89e, 0x5f058808, 0xc60cd9b2, 0xb10be924,
0x2f6f7c87, 0x58684c11, 0xc1611dab, 0xb6662d3d, 0x76dc4190, 0x01db7106,
0x98d220bc, 0xefd5102a, 0x71b18589, 0x06b6b51f, 0x9fbfe4a5, 0xe8b8d433,
0x7807c9a2, 0x0f00f934, 0x9609a88e, 0xe10e9818, 0x7f6a0dbb, 0x086d3d2d,
0x91646c97, 0xe6635c01, 0x6b6b51f4, 0x1c6c6162, 0x856530d8, 0xf262004e,
0x6c0695ed, 0x1b01a57b, 0x8208f4c1, 0xf50fc457, 0x65b0d9c6, 0x12b7e950,
0x8bbeb8ea, 0xfcb9887c, 0x62dd1ddf, 0x15da2d49, 0x8cd37cf3, 0xfbd44c65,
0x4db26158, 0x3ab551ce, 0xa3bc0074, 0xd4bb30e2, 0x4adfa541, 0x3dd895d7,
0xa4d1c46d, 0xd3d6f4fb, 0x4369e96a, 0x346ed9fc, 0xad678846, 0xda60b8d0,
0x44042d73, 0x33031de5, 0xaa0a4c5f, 0xdd0d7cc9, 0x5005713c, 0x270241aa,
0xbe0b1010, 0xc90c2086, 0x5768b525, 0x206f85b3, 0xb966d409, 0xce61e49f,
0x5edef90e, 0x29d9c998, 0xb0d09822, 0xc7d7a8b4, 0x59b33d17, 0x2eb40d81,
0xb7bd5c3b, 0xc0ba6cad, 0xedb88320, 0x9abfb3b6, 0x03b6e20c, 0x74b1d29a,
0xead54739, 0x9dd277af, 0x04db2615, 0x73dc1683, 0xe3630b12, 0x94643b84,
0x0d6d6a3e, 0x7a6a5aa8, 0xe40ecf0b, 0x9309ff9d, 0x0a00ae27, 0x7d079eb1,
0xf00f9344, 0x8708a3d2, 0x1e01f268, 0x6906c2fe, 0xf762575d, 0x806567cb,
0x196c3671, 0x6e6b06e7, 0xfed41b76, 0x89d32be0, 0x10da7a5a, 0x67dd4acc,
0xf9b9df6f, 0x8ebeeff9, 0x17b7be43, 0x60b08ed5, 0xd6d6a3e8, 0xa1d1937e,
0x38d8c2c4, 0x4fdff252, 0xd1bb67f1, 0xa6bc5767, 0x3fb506dd, 0x48b2364b,
0xd80d2bda, 0xaf0a1b4c, 0x36034af6, 0x41047a60, 0xdf60efc3, 0xa867df55,
0x316e8eef, 0x4669be79, 0xcb61b38c, 0xbc66831a, 0x256fd2a0, 0x5268e236,
0xcc0c7795, 0xbb0b4703, 0x220216b9, 0x5505262f, 0xc5ba3bbe, 0xb2bd0b28,
0x2bb45a92, 0x5cb36a04, 0xc2d7ffa7, 0xb5d0cf31, 0x2cd99e8b, 0x5bdeae1d,
0x9b64c2b0, 0xec63f226, 0x756aa39c, 0x026d930a, 0x9c0906a9, 0xeb0e363f,
0x72076785, 0x05005713, 0x95bf4a82, 0xe2b87a14, 0x7bb12bae, 0x0cb61b38,
0x92d28e9b, 0xe5d5be0d, 0x7cdcefb7, 0x0bdbdf21, 0x86d3d2d4, 0xf1d4e242,
0x68ddb3f8, 0x1fda836e, 0x81be16cd, 0xf6b9265b, 0x6fb077e1, 0x18b74777,
0x88085ae6, 0xff0f6a70, 0x66063bca, 0x11010b5c, 0x8f659eff, 0xf862ae69,
0x616bffd3, 0x166ccf45, 0xa00ae278, 0xd70dd2ee, 0x4e048354, 0x3903b3c2,
0xa7672661, 0xd06016f7, 0x4969474d, 0x3e6e77db, 0xaed16a4a, 0xd9d65adc,
0x40df0b66, 0x37d83bf0, 0xa9bcae53, 0xdebb9ec5, 0x47b2cf7f, 0x30b5ffe9,
0xbdbdf21c, 0xcabac28a, 0x53b39330, 0x24b4a3a6, 0xbad03605, 0xcdd70693,
0x54de5729, 0x23d967bf, 0xb3667a2e, 0xc4614ab8, 0x5d681b02, 0x2a6f2b94,
0xb40bbe37, 0xc30c8ea1, 0x5a05df1b, 0x2d02ef8d
};

#define UPDC32(octet,crc) (crc_32_tab[((crc)\
     ^ ((BYTE)octet)) & 0xff] ^ ((crc) >> 8))


DWORD crc32buf(char *buf, size_t len)
{
      register DWORD oldcrc32;

      oldcrc32 = 0xFFFFFFFF;

      for ( ; len; --len, ++buf)
      {
            oldcrc32 = UPDC32(*buf, oldcrc32);
      }

      return ~oldcrc32;

}

  int sqlite3PcacheTrace = 2;       /* 0: off  1: simple  2: cache dumps */
  int sqlite3PcacheMxDump = 9999;   /* Max cache entries for pcacheDump() */
# define pcacheTrace(X) if(sqlite3PcacheTrace){sqlite3DebugPrintf X;}
  void pcacheDump(PCache *pCache){
    int N;
    int i, j;
    sqlite3_pcache_page *pLower;
    PgHdr *pPg;
    unsigned char *a;
  
    if( sqlite3PcacheTrace<2 ) return;
    if( pCache->pCache==0 ) return;
    N = sqlite3PcachePagecount(pCache);
    if( N>sqlite3PcacheMxDump ) N = sqlite3PcacheMxDump;
    for(i=1; i<=N; i++){
       pLower = sqlite3GlobalConfig.pcache2.xFetch(pCache->pCache, i, 0);
       if( pLower==0 ) continue;
       pPg = (PgHdr*)pLower->pExtra;
       printf("%3d: nRef %2d flgs %02x data ", i, pPg->nRef, pPg->flags);
       a = (unsigned char *)pLower->pBuf;
       for(j=0; j<12; j++) printf("%02x", a[j]);
       DWORD crcsum = crc32buf(a, 4096);
       printf(" ");
       printf("%08lx", crcsum & 0xffffffff);
       printf("\n");
       if( pPg->pPage==0 ){
         sqlite3GlobalConfig.pcache2.xUnpin(pCache->pCache, pLower, 0);
       }
    }
  }
  #else
# define pcacheTrace(X)
# define pcacheDump(X)
#endif

/*
** Check invariants on a PgHdr entry.  Return true if everything is OK.
** Return false if any invariant is violated.
**
** This routine is for use inside of assert() statements only.  For
** example:
**
**          assert( sqlite3PcachePageSanity(pPg) );
*/
#ifdef SQLITE_DEBUG
int sqlite3PcachePageSanity(PgHdr *pPg){
  PCache *pCache;
  assert( pPg!=0 );
  assert( pPg->pgno>0 || pPg->pPager==0 );    /* Page number is 1 or more */
  pCache = pPg->pCache;
  assert( pCache!=0 );      /* Every page has an associated PCache */
  if( pPg->flags & PGHDR_CLEAN ){
    assert( (pPg->flags & PGHDR_DIRTY)==0 );/* Cannot be both CLEAN and DIRTY */
    assert( pCache->pDirty!=pPg );          /* CLEAN pages not on dirty list */
    assert( pCache->pDirtyTail!=pPg );
  }
  /* WRITEABLE pages must also be DIRTY */
  if( pPg->flags & PGHDR_WRITEABLE ){
    assert( pPg->flags & PGHDR_DIRTY );     /* WRITEABLE implies DIRTY */
  }
  /* NEED_SYNC can be set independently of WRITEABLE.  This can happen,
  ** for example, when using the sqlite3PagerDontWrite() optimization:
  **    (1)  Page X is journalled, and gets WRITEABLE and NEED_SEEK.
  **    (2)  Page X moved to freelist, WRITEABLE is cleared
  **    (3)  Page X reused, WRITEABLE is set again
  ** If NEED_SYNC had been cleared in step 2, then it would not be reset
  ** in step 3, and page might be written into the database without first
  ** syncing the rollback journal, which might cause corruption on a power
  ** loss.
  **
  ** Another example is when the database page size is smaller than the
  ** disk sector size.  When any page of a sector is journalled, all pages
  ** in that sector are marked NEED_SYNC even if they are still CLEAN, just
  ** in case they are later modified, since all pages in the same sector
  ** must be journalled and synced before any of those pages can be safely
  ** written.
  */
  return 1;
}
#endif /* SQLITE_DEBUG */


/********************************** Linked List Management ********************/

/* Allowed values for second argument to pcacheManageDirtyList() */
#define PCACHE_DIRTYLIST_REMOVE   1    /* Remove pPage from dirty list */
#define PCACHE_DIRTYLIST_ADD      2    /* Add pPage to the dirty list */
#define PCACHE_DIRTYLIST_FRONT    3    /* Move pPage to the front of the list */

/*
** Manage pPage's participation on the dirty list.  Bits of the addRemove
** argument determines what operation to do.  The 0x01 bit means first
** remove pPage from the dirty list.  The 0x02 means add pPage back to
** the dirty list.  Doing both moves pPage to the front of the dirty list.
*/
static void pcacheManageDirtyList(PgHdr *pPage, u8 addRemove){
  PCache *p = pPage->pCache;

  pcacheTrace(("%p.DIRTYLIST.%s %d\n", p,
                addRemove==1 ? "REMOVE" : addRemove==2 ? "ADD" : "FRONT",
                pPage->pgno));
  if( addRemove & PCACHE_DIRTYLIST_REMOVE ){
    assert( pPage->pDirtyNext || pPage==p->pDirtyTail );
    assert( pPage->pDirtyPrev || pPage==p->pDirty );
  
    /* Update the PCache1.pSynced variable if necessary. */
    if( p->pSynced==pPage ){
      p->pSynced = pPage->pDirtyPrev;
    }
  
    if( pPage->pDirtyNext ){
      pPage->pDirtyNext->pDirtyPrev = pPage->pDirtyPrev;
    }else{
      assert( pPage==p->pDirtyTail );
      p->pDirtyTail = pPage->pDirtyPrev;
    }
    if( pPage->pDirtyPrev ){
      pPage->pDirtyPrev->pDirtyNext = pPage->pDirtyNext;
    }else{
      /* If there are now no dirty pages in the cache, set eCreate to 2. 
      ** This is an optimization that allows sqlite3PcacheFetch() to skip
      ** searching for a dirty page to eject from the cache when it might
      ** otherwise have to.  */
      assert( pPage==p->pDirty );
      p->pDirty = pPage->pDirtyNext;
      assert( p->bPurgeable || p->eCreate==2 );
      if( p->pDirty==0 ){         /*OPTIMIZATION-IF-TRUE*/
        assert( p->bPurgeable==0 || p->eCreate==1 );
        p->eCreate = 2;
      }
    }
  }
  if( addRemove & PCACHE_DIRTYLIST_ADD ){
    pPage->pDirtyPrev = 0;
    pPage->pDirtyNext = p->pDirty;
    if( pPage->pDirtyNext ){
      assert( pPage->pDirtyNext->pDirtyPrev==0 );
      pPage->pDirtyNext->pDirtyPrev = pPage;
    }else{
      p->pDirtyTail = pPage;
      if( p->bPurgeable ){
        assert( p->eCreate==2 );
        p->eCreate = 1;
      }
    }
    p->pDirty = pPage;

    /* If pSynced is NULL and this page has a clear NEED_SYNC flag, set
    ** pSynced to point to it. Checking the NEED_SYNC flag is an 
    ** optimization, as if pSynced points to a page with the NEED_SYNC
    ** flag set sqlite3PcacheFetchStress() searches through all newer 
    ** entries of the dirty-list for a page with NEED_SYNC clear anyway.  */
    if( !p->pSynced 
     && 0==(pPage->flags&PGHDR_NEED_SYNC)   /*OPTIMIZATION-IF-FALSE*/
    ){
      p->pSynced = pPage;
    }
  }
  pcacheDump(p);
}

/*
** Wrapper around the pluggable caches xUnpin method. If the cache is
** being used for an in-memory database, this function is a no-op.
*/
static void pcacheUnpin(PgHdr *p){
  if( p->pCache->bPurgeable ){
    pcacheTrace(("%p.UNPIN %d\n", p->pCache, p->pgno));
    sqlite3GlobalConfig.pcache2.xUnpin(p->pCache->pCache, p->pPage, 0);
    pcacheDump(p->pCache);
  }
}

/*
** Compute the number of pages of cache requested.   p->szCache is the
** cache size requested by the "PRAGMA cache_size" statement.
*/
static int numberOfCachePages(PCache *p){
  if( p->szCache>=0 ){
    /* IMPLEMENTATION-OF: R-42059-47211 If the argument N is positive then the
    ** suggested cache size is set to N. */
    return p->szCache;
  }else{
    /* IMPLEMENTATION-OF: R-61436-13639 If the argument N is negative, then
    ** the number of cache pages is adjusted to use approximately abs(N*1024)
    ** bytes of memory. */
    return (int)((-1024*(i64)p->szCache)/(p->szPage+p->szExtra));
  }
}

/*************************************************** General Interfaces ******
**
** Initialize and shutdown the page cache subsystem. Neither of these 
** functions are threadsafe.
*/
int sqlite3PcacheInitialize(void){
  if( sqlite3GlobalConfig.pcache2.xInit==0 ){
    /* IMPLEMENTATION-OF: R-26801-64137 If the xInit() method is NULL, then the
    ** built-in default page cache is used instead of the application defined
    ** page cache. */
    sqlite3PCacheSetDefault();
  }
  return sqlite3GlobalConfig.pcache2.xInit(sqlite3GlobalConfig.pcache2.pArg);
}
void sqlite3PcacheShutdown(void){
  if( sqlite3GlobalConfig.pcache2.xShutdown ){
    /* IMPLEMENTATION-OF: R-26000-56589 The xShutdown() method may be NULL. */
    sqlite3GlobalConfig.pcache2.xShutdown(sqlite3GlobalConfig.pcache2.pArg);
  }
}

/*
** Return the size in bytes of a PCache object.
*/
int sqlite3PcacheSize(void){ return sizeof(PCache); }

/*
** Create a new PCache object. Storage space to hold the object
** has already been allocated and is passed in as the p pointer. 
** The caller discovers how much space needs to be allocated by 
** calling sqlite3PcacheSize().
**
** szExtra is some extra space allocated for each page.  The first
** 8 bytes of the extra space will be zeroed as the page is allocated,
** but remaining content will be uninitialized.  Though it is opaque
** to this module, the extra space really ends up being the MemPage
** structure in the pager.
*/
int sqlite3PcacheOpen(
  int szPage,                  /* Size of every page */
  int szExtra,                 /* Extra space associated with each page */
  int bPurgeable,              /* True if pages are on backing store */
  int (*xStress)(void*,PgHdr*),/* Call to try to make pages clean */
  void *pStress,               /* Argument to xStress */
  PCache *p                    /* Preallocated space for the PCache */
){
  memset(p, 0, sizeof(PCache));
  p->szPage = 1;
  p->szExtra = szExtra;
  assert( szExtra>=8 );  /* First 8 bytes will be zeroed */
  p->bPurgeable = bPurgeable;
  p->eCreate = 2;
  p->xStress = xStress;
  p->pStress = pStress;
  p->szCache = 100;
  p->szSpill = 1;
  pcacheTrace(("%p.OPEN szPage %d bPurgeable %d\n",p,szPage,bPurgeable));
  return sqlite3PcacheSetPageSize(p, szPage);
}

/*
** Change the page size for PCache object. The caller must ensure that there
** are no outstanding page references when this function is called.
*/
int sqlite3PcacheSetPageSize(PCache *pCache, int szPage){
  assert( pCache->nRefSum==0 && pCache->pDirty==0 );
  if( pCache->szPage ){
    sqlite3_pcache *pNew;
    pNew = sqlite3GlobalConfig.pcache2.xCreate(
                szPage, pCache->szExtra + ROUND8(sizeof(PgHdr)),
                pCache->bPurgeable
    );
    if( pNew==0 ) return SQLITE_NOMEM_BKPT;
    sqlite3GlobalConfig.pcache2.xCachesize(pNew, numberOfCachePages(pCache));
    if( pCache->pCache ){
      sqlite3GlobalConfig.pcache2.xDestroy(pCache->pCache);
    }
    pCache->pCache = pNew;
    pCache->szPage = szPage;
    pcacheTrace(("%p.PAGESIZE %d\n",pCache,szPage));
  }
  return SQLITE_OK;
}

/*
** Try to obtain a page from the cache.
**
** This routine returns a pointer to an sqlite3_pcache_page object if
** such an object is already in cache, or if a new one is created.
** This routine returns a NULL pointer if the object was not in cache
** and could not be created.
**
** The createFlags should be 0 to check for existing pages and should
** be 3 (not 1, but 3) to try to create a new page.
**
** If the createFlag is 0, then NULL is always returned if the page
** is not already in the cache.  If createFlag is 1, then a new page
** is created only if that can be done without spilling dirty pages
** and without exceeding the cache size limit.
**
** The caller needs to invoke sqlite3PcacheFetchFinish() to properly
** initialize the sqlite3_pcache_page object and convert it into a
** PgHdr object.  The sqlite3PcacheFetch() and sqlite3PcacheFetchFinish()
** routines are split this way for performance reasons. When separated
** they can both (usually) operate without having to push values to
** the stack on entry and pop them back off on exit, which saves a
** lot of pushing and popping.
*/
sqlite3_pcache_page *sqlite3PcacheFetch(
  PCache *pCache,       /* Obtain the page from this cache */
  Pgno pgno,            /* Page number to obtain */
  int createFlag        /* If true, create page if it does not exist already */
){
  printf("pager - sqlite3PcacheFetch %d\n", pgno);
  int eCreate;
  sqlite3_pcache_page *pRes;

  assert( pCache!=0 );
  assert( pCache->pCache!=0 );
  assert( createFlag==3 || createFlag==0 );
  assert( pCache->eCreate==((pCache->bPurgeable && pCache->pDirty) ? 1 : 2) );

  /* eCreate defines what to do if the page does not exist.
  **    0     Do not allocate a new page.  (createFlag==0)
  **    1     Allocate a new page if doing so is inexpensive.
  **          (createFlag==1 AND bPurgeable AND pDirty)
  **    2     Allocate a new page even it doing so is difficult.
  **          (createFlag==1 AND !(bPurgeable AND pDirty)
  */
  eCreate = createFlag & pCache->eCreate;
  assert( eCreate==0 || eCreate==1 || eCreate==2 );
  assert( createFlag==0 || pCache->eCreate==eCreate );
  assert( createFlag==0 || eCreate==1+(!pCache->bPurgeable||!pCache->pDirty) );
  pRes = sqlite3GlobalConfig.pcache2.xFetch(pCache->pCache, pgno, eCreate);
  pcacheTrace(("%p.FETCH %d%s (result: %p)\n",pCache,pgno,
               createFlag?" create":"",pRes));
  return pRes;
}

/*
** If the sqlite3PcacheFetch() routine is unable to allocate a new
** page because no clean pages are available for reuse and the cache
** size limit has been reached, then this routine can be invoked to 
** try harder to allocate a page.  This routine might invoke the stress
** callback to spill dirty pages to the journal.  It will then try to
** allocate the new page and will only fail to allocate a new page on
** an OOM error.
**
** This routine should be invoked only after sqlite3PcacheFetch() fails.
*/
int sqlite3PcacheFetchStress(
  PCache *pCache,                 /* Obtain the page from this cache */
  Pgno pgno,                      /* Page number to obtain */
  sqlite3_pcache_page **ppPage    /* Write result here */
){
  PgHdr *pPg;
  if( pCache->eCreate==2 ) return 0;

  if( sqlite3PcachePagecount(pCache)>pCache->szSpill ){
    /* Find a dirty page to write-out and recycle. First try to find a 
    ** page that does not require a journal-sync (one with PGHDR_NEED_SYNC
    ** cleared), but if that is not possible settle for any other 
    ** unreferenced dirty page.
    **
    ** If the LRU page in the dirty list that has a clear PGHDR_NEED_SYNC
    ** flag is currently referenced, then the following may leave pSynced
    ** set incorrectly (pointing to other than the LRU page with NEED_SYNC
    ** cleared). This is Ok, as pSynced is just an optimization.  */
    for(pPg=pCache->pSynced; 
        pPg && (pPg->nRef || (pPg->flags&PGHDR_NEED_SYNC)); 
        pPg=pPg->pDirtyPrev
    );
    pCache->pSynced = pPg;
    if( !pPg ){
      for(pPg=pCache->pDirtyTail; pPg && pPg->nRef; pPg=pPg->pDirtyPrev);
    }
    if( pPg ){
      int rc;
#ifdef SQLITE_LOG_CACHE_SPILL
      sqlite3_log(SQLITE_FULL, 
                  "spill page %d making room for %d - cache used: %d/%d",
                  pPg->pgno, pgno,
                  sqlite3GlobalConfig.pcache.xPagecount(pCache->pCache),
                numberOfCachePages(pCache));
#endif
      pcacheTrace(("%p.SPILL %d\n",pCache,pPg->pgno));
      rc = pCache->xStress(pCache->pStress, pPg);
      pcacheDump(pCache);
      if( rc!=SQLITE_OK && rc!=SQLITE_BUSY ){
        return rc;
      }
    }
  }
  *ppPage = sqlite3GlobalConfig.pcache2.xFetch(pCache->pCache, pgno, 2);
  return *ppPage==0 ? SQLITE_NOMEM_BKPT : SQLITE_OK;
}

/*
** This is a helper routine for sqlite3PcacheFetchFinish()
**
** In the uncommon case where the page being fetched has not been
** initialized, this routine is invoked to do the initialization.
** This routine is broken out into a separate function since it
** requires extra stack manipulation that can be avoided in the common
** case.
*/
static SQLITE_NOINLINE PgHdr *pcacheFetchFinishWithInit(
  PCache *pCache,             /* Obtain the page from this cache */
  Pgno pgno,                  /* Page number obtained */
  sqlite3_pcache_page *pPage  /* Page obtained by prior PcacheFetch() call */
){
  PgHdr *pPgHdr;
  assert( pPage!=0 );
  pPgHdr = (PgHdr*)pPage->pExtra;
  assert( pPgHdr->pPage==0 );
  memset(&pPgHdr->pDirty, 0, sizeof(PgHdr) - offsetof(PgHdr,pDirty));
  pPgHdr->pPage = pPage;
  pPgHdr->pData = pPage->pBuf;
  pPgHdr->pExtra = (void *)&pPgHdr[1];
  memset(pPgHdr->pExtra, 0, 8);
  pPgHdr->pCache = pCache;
  pPgHdr->pgno = pgno;
  pPgHdr->flags = PGHDR_CLEAN;
  return sqlite3PcacheFetchFinish(pCache,pgno,pPage);
}

/*
** This routine converts the sqlite3_pcache_page object returned by
** sqlite3PcacheFetch() into an initialized PgHdr object.  This routine
** must be called after sqlite3PcacheFetch() in order to get a usable
** result.
*/
PgHdr *sqlite3PcacheFetchFinish(
  PCache *pCache,             /* Obtain the page from this cache */
  Pgno pgno,                  /* Page number obtained */
  sqlite3_pcache_page *pPage  /* Page obtained by prior PcacheFetch() call */
){
  printf("pager - sqlite3PcacheFetch %d\n", pgno);
  PgHdr *pPgHdr;

  assert( pPage!=0 );
  pPgHdr = (PgHdr *)pPage->pExtra;

  if( !pPgHdr->pPage ){
    return pcacheFetchFinishWithInit(pCache, pgno, pPage);
  }
  pCache->nRefSum++;
  pPgHdr->nRef++;
  assert( sqlite3PcachePageSanity(pPgHdr) );
  return pPgHdr;
}

/*
** Decrement the reference count on a page. If the page is clean and the
** reference count drops to 0, then it is made eligible for recycling.
*/
void SQLITE_NOINLINE sqlite3PcacheRelease(PgHdr *p){
  assert( p->nRef>0 );
  p->pCache->nRefSum--;
  if( (--p->nRef)==0 ){
    if( p->flags&PGHDR_CLEAN ){
      pcacheUnpin(p);
    }else{
      pcacheManageDirtyList(p, PCACHE_DIRTYLIST_FRONT);
    }
  }
}

/*
** Increase the reference count of a supplied page by 1.
*/
void sqlite3PcacheRef(PgHdr *p){
  assert(p->nRef>0);
  assert( sqlite3PcachePageSanity(p) );
  p->nRef++;
  p->pCache->nRefSum++;
}

/*
** Drop a page from the cache. There must be exactly one reference to the
** page. This function deletes that reference, so after it returns the
** page pointed to by p is invalid.
*/
void sqlite3PcacheDrop(PgHdr *p){
  assert( p->nRef==1 );
  assert( sqlite3PcachePageSanity(p) );
  if( p->flags&PGHDR_DIRTY ){
    pcacheManageDirtyList(p, PCACHE_DIRTYLIST_REMOVE);
  }
  p->pCache->nRefSum--;
  sqlite3GlobalConfig.pcache2.xUnpin(p->pCache->pCache, p->pPage, 1);
}

/*
** Make sure the page is marked as dirty. If it isn't dirty already,
** make it so.
*/
void sqlite3PcacheMakeDirty(PgHdr *p){
  assert( p->nRef>0 );
  assert( sqlite3PcachePageSanity(p) );
  if( p->flags & (PGHDR_CLEAN|PGHDR_DONT_WRITE) ){    /*OPTIMIZATION-IF-FALSE*/
    p->flags &= ~PGHDR_DONT_WRITE;
    if( p->flags & PGHDR_CLEAN ){
      p->flags ^= (PGHDR_DIRTY|PGHDR_CLEAN);
      pcacheTrace(("%p.DIRTY %d\n",p->pCache,p->pgno));
      assert( (p->flags & (PGHDR_DIRTY|PGHDR_CLEAN))==PGHDR_DIRTY );
      pcacheManageDirtyList(p, PCACHE_DIRTYLIST_ADD);
    }
    assert( sqlite3PcachePageSanity(p) );
  }
}

/*
** Make sure the page is marked as clean. If it isn't clean already,
** make it so.
*/
void sqlite3PcacheMakeClean(PgHdr *p){
  assert( sqlite3PcachePageSanity(p) );
  if( ALWAYS((p->flags & PGHDR_DIRTY)!=0) ){
    assert( (p->flags & PGHDR_CLEAN)==0 );
    pcacheManageDirtyList(p, PCACHE_DIRTYLIST_REMOVE);
    p->flags &= ~(PGHDR_DIRTY|PGHDR_NEED_SYNC|PGHDR_WRITEABLE);
    p->flags |= PGHDR_CLEAN;
    pcacheTrace(("%p.CLEAN %d\n",p->pCache,p->pgno));
    assert( sqlite3PcachePageSanity(p) );
    if( p->nRef==0 ){
      pcacheUnpin(p);
    }
  }
}

/*
** Make every page in the cache clean.
*/
void sqlite3PcacheCleanAll(PCache *pCache){
  PgHdr *p;
  pcacheTrace(("%p.CLEAN-ALL\n",pCache));
  while( (p = pCache->pDirty)!=0 ){
    sqlite3PcacheMakeClean(p);
  }
}

/*
** Clear the PGHDR_NEED_SYNC and PGHDR_WRITEABLE flag from all dirty pages.
*/
void sqlite3PcacheClearWritable(PCache *pCache){
  PgHdr *p;
  pcacheTrace(("%p.CLEAR-WRITEABLE\n",pCache));
  for(p=pCache->pDirty; p; p=p->pDirtyNext){
    p->flags &= ~(PGHDR_NEED_SYNC|PGHDR_WRITEABLE);
  }
  pCache->pSynced = pCache->pDirtyTail;
}

/*
** Clear the PGHDR_NEED_SYNC flag from all dirty pages.
*/
void sqlite3PcacheClearSyncFlags(PCache *pCache){
  PgHdr *p;
  for(p=pCache->pDirty; p; p=p->pDirtyNext){
    p->flags &= ~PGHDR_NEED_SYNC;
  }
  pCache->pSynced = pCache->pDirtyTail;
}

/*
** Change the page number of page p to newPgno. 
*/
void sqlite3PcacheMove(PgHdr *p, Pgno newPgno){
  PCache *pCache = p->pCache;
  assert( p->nRef>0 );
  assert( newPgno>0 );
  assert( sqlite3PcachePageSanity(p) );
  pcacheTrace(("%p.MOVE %d -> %d\n",pCache,p->pgno,newPgno));
  sqlite3GlobalConfig.pcache2.xRekey(pCache->pCache, p->pPage, p->pgno,newPgno);
  p->pgno = newPgno;
  if( (p->flags&PGHDR_DIRTY) && (p->flags&PGHDR_NEED_SYNC) ){
    pcacheManageDirtyList(p, PCACHE_DIRTYLIST_FRONT);
  }
}

/*
** Drop every cache entry whose page number is greater than "pgno". The
** caller must ensure that there are no outstanding references to any pages
** other than page 1 with a page number greater than pgno.
**
** If there is a reference to page 1 and the pgno parameter passed to this
** function is 0, then the data area associated with page 1 is zeroed, but
** the page object is not dropped.
*/
void sqlite3PcacheTruncate(PCache *pCache, Pgno pgno){
  if( pCache->pCache ){
    PgHdr *p;
    PgHdr *pNext;
    pcacheTrace(("%p.TRUNCATE %d\n",pCache,pgno));
    for(p=pCache->pDirty; p; p=pNext){
      pNext = p->pDirtyNext;
      /* This routine never gets call with a positive pgno except right
      ** after sqlite3PcacheCleanAll().  So if there are dirty pages,
      ** it must be that pgno==0.
      */
      assert( p->pgno>0 );
      if( p->pgno>pgno ){
        assert( p->flags&PGHDR_DIRTY );
        sqlite3PcacheMakeClean(p);
      }
    }
    if( pgno==0 && pCache->nRefSum ){
      sqlite3_pcache_page *pPage1;
      pPage1 = sqlite3GlobalConfig.pcache2.xFetch(pCache->pCache,1,0);
      if( ALWAYS(pPage1) ){  /* Page 1 is always available in cache, because
                             ** pCache->nRefSum>0 */
        memset(pPage1->pBuf, 0, pCache->szPage);
        pgno = 1;
      }
    }
    sqlite3GlobalConfig.pcache2.xTruncate(pCache->pCache, pgno+1);
  }
}

/*
** Close a cache.
*/
void sqlite3PcacheClose(PCache *pCache){
  assert( pCache->pCache!=0 );
  pcacheTrace(("%p.CLOSE\n",pCache));
  sqlite3GlobalConfig.pcache2.xDestroy(pCache->pCache);
}

/* 
** Discard the contents of the cache.
*/
void sqlite3PcacheClear(PCache *pCache){
  sqlite3PcacheTruncate(pCache, 0);
}

/*
** Merge two lists of pages connected by pDirty and in pgno order.
** Do not bother fixing the pDirtyPrev pointers.
*/
static PgHdr *pcacheMergeDirtyList(PgHdr *pA, PgHdr *pB){
  PgHdr result, *pTail;
  pTail = &result;
  assert( pA!=0 && pB!=0 );
  for(;;){
    if( pA->pgno<pB->pgno ){
      pTail->pDirty = pA;
      pTail = pA;
      pA = pA->pDirty;
      if( pA==0 ){
        pTail->pDirty = pB;
        break;
      }
    }else{
      pTail->pDirty = pB;
      pTail = pB;
      pB = pB->pDirty;
      if( pB==0 ){
        pTail->pDirty = pA;
        break;
      }
    }
  }
  return result.pDirty;
}

/*
** Sort the list of pages in accending order by pgno.  Pages are
** connected by pDirty pointers.  The pDirtyPrev pointers are
** corrupted by this sort.
**
** Since there cannot be more than 2^31 distinct pages in a database,
** there cannot be more than 31 buckets required by the merge sorter.
** One extra bucket is added to catch overflow in case something
** ever changes to make the previous sentence incorrect.
*/
#define N_SORT_BUCKET  32
static PgHdr *pcacheSortDirtyList(PgHdr *pIn){
  PgHdr *a[N_SORT_BUCKET], *p;
  int i;
  memset(a, 0, sizeof(a));
  while( pIn ){
    p = pIn;
    pIn = p->pDirty;
    p->pDirty = 0;
    for(i=0; ALWAYS(i<N_SORT_BUCKET-1); i++){
      if( a[i]==0 ){
        a[i] = p;
        break;
      }else{
        p = pcacheMergeDirtyList(a[i], p);
        a[i] = 0;
      }
    }
    if( NEVER(i==N_SORT_BUCKET-1) ){
      /* To get here, there need to be 2^(N_SORT_BUCKET) elements in
      ** the input list.  But that is impossible.
      */
      a[i] = pcacheMergeDirtyList(a[i], p);
    }
  }
  p = a[0];
  for(i=1; i<N_SORT_BUCKET; i++){
    if( a[i]==0 ) continue;
    p = p ? pcacheMergeDirtyList(p, a[i]) : a[i];
  }
  return p;
}

/*
** Return a list of all dirty pages in the cache, sorted by page number.
*/
PgHdr *sqlite3PcacheDirtyList(PCache *pCache){
  PgHdr *p;
  for(p=pCache->pDirty; p; p=p->pDirtyNext){
    p->pDirty = p->pDirtyNext;
  }
  return pcacheSortDirtyList(pCache->pDirty);
}

/* 
** Return the total number of references to all pages held by the cache.
**
** This is not the total number of pages referenced, but the sum of the
** reference count for all pages.
*/
int sqlite3PcacheRefCount(PCache *pCache){
  return pCache->nRefSum;
}

/*
** Return the number of references to the page supplied as an argument.
*/
int sqlite3PcachePageRefcount(PgHdr *p){
  return p->nRef;
}

/* 
** Return the total number of pages in the cache.
*/
int sqlite3PcachePagecount(PCache *pCache){
  assert( pCache->pCache!=0 );
  return sqlite3GlobalConfig.pcache2.xPagecount(pCache->pCache);
}

#ifdef SQLITE_TEST
/*
** Get the suggested cache-size value.
*/
int sqlite3PcacheGetCachesize(PCache *pCache){
  return numberOfCachePages(pCache);
}
#endif

/*
** Set the suggested cache-size value.
*/
void sqlite3PcacheSetCachesize(PCache *pCache, int mxPage){
  assert( pCache->pCache!=0 );
  pCache->szCache = mxPage;
  sqlite3GlobalConfig.pcache2.xCachesize(pCache->pCache,
                                         numberOfCachePages(pCache));
}

/*
** Set the suggested cache-spill value.  Make no changes if if the
** argument is zero.  Return the effective cache-spill size, which will
** be the larger of the szSpill and szCache.
*/
int sqlite3PcacheSetSpillsize(PCache *p, int mxPage){
  int res;
  assert( p->pCache!=0 );
  if( mxPage ){
    if( mxPage<0 ){
      mxPage = (int)((-1024*(i64)mxPage)/(p->szPage+p->szExtra));
    }
    p->szSpill = mxPage;
  }
  res = numberOfCachePages(p);
  if( res<p->szSpill ) res = p->szSpill; 
  return res;
}

/*
** Free up as much memory as possible from the page cache.
*/
void sqlite3PcacheShrink(PCache *pCache){
  assert( pCache->pCache!=0 );
  sqlite3GlobalConfig.pcache2.xShrink(pCache->pCache);
}

/*
** Return the size of the header added by this middleware layer
** in the page-cache hierarchy.
*/
int sqlite3HeaderSizePcache(void){ return ROUND8(sizeof(PgHdr)); }

/*
** Return the number of dirty pages currently in the cache, as a percentage
** of the configured cache size.
*/
int sqlite3PCachePercentDirty(PCache *pCache){
  PgHdr *pDirty;
  int nDirty = 0;
  int nCache = numberOfCachePages(pCache);
  for(pDirty=pCache->pDirty; pDirty; pDirty=pDirty->pDirtyNext) nDirty++;
  return nCache ? (int)(((i64)nDirty * 100) / nCache) : 0;
}

#if defined(SQLITE_CHECK_PAGES) || defined(SQLITE_DEBUG)
/*
** For all dirty pages currently in the cache, invoke the specified
** callback. This is only used if the SQLITE_CHECK_PAGES macro is
** defined.
*/
void sqlite3PcacheIterateDirty(PCache *pCache, void (*xIter)(PgHdr *)){
  PgHdr *pDirty;
  for(pDirty=pCache->pDirty; pDirty; pDirty=pDirty->pDirtyNext){
    xIter(pDirty);
  }
}
#endif
