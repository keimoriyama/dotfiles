function obsidian#set_config(vaultDir, value='')abort
	call add(g:obsidian#config, a:vaultDir, a:value)
endfunction
