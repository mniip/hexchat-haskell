#define _GNU_SOURCE
#include <stdio.h>
#include <hexchat-plugin.h>

#include "HsFFI.h"

#include "HexChat/Linker_stub.h"

extern void __stginit_HexChatziLinker(void);

int hexchat_plugin_init(hexchat_plugin *plugin, char **name, char **desc, char **ver, char *arg)
{
	int argc = 0;
	char *argva[] = {NULL};
	char **argv = argva;
	hs_init(&argc, &argv);
	hs_add_root(__stginit_HexChatziLinker);

	int result = plugin_init(plugin, name, desc, ver, arg);
	if(!result)
		hs_exit();
	return result;
}

int hexchat_plugin_deinit(hexchat_plugin *plugin)
{
	int result = plugin_deinit(plugin);
	if(result)
		hs_exit();
	return result;
}
