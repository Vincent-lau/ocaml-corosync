#include <stdio.h>

#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <errno.h>
#include <poll.h>

#include <corosync/corotypes.h>
#include <qb/qbipcc.h>
#include <corosync/corodefs.h>
#include <corosync/cmap.h>
#include <corosync/hdb.h>
#include <corosync/quorum.h>

#include <corosync/cmap.h>


#include <stdio.h>
#include <sys/wait.h>
#include <signal.h>


int main(void) {
  cs_error_t err;
  cmap_handle_t handle;
  
  err = cmap_initialize(&handle);

  char *str;
  const char *key_name = "totem.cluster_name";

  cmap_get_string(handle, key_name, &str);

  cmap_finalize(handle);

  printf("%s", str);


  return 0;

}
