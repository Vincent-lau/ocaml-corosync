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

#include <netinet/in.h>


int main(void) {
  // cs_error_t err;
  // cmap_handle_t handle;
  
  // err = cmap_initialize(&handle);

  // char *str;

  // cmap_iter_handle_t iter_handle;
  // cmap_iter_init(handle, "totem", &iter_handle);
  // char key_name[256];
  // size_t value_len;
  // cmap_value_types_t type;
  // cmap_iter_next(handle, iter_handle, key_name, &value_len, &type);
  // cmap_finalize(handle);

  printf("%ld", sizeof(struct sockaddr_in6));

  return 0;

}
