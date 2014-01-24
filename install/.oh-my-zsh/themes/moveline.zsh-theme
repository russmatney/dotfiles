# ------------------------------------------------------------------------
# Christopher Garvis
# oh-my-zsh theme
# Totally ripped-off Tyler Cipriani
# ------------------------------------------------------------------------

# Grab the current date (%W) and time (%t):
#bold MOVELINE_TIME_="%{$fg_bold[red]%}#%{$fg_bold[white]%}( %{$fg_bold[yellow]%}%W%{$reset_color%}@%{$fg_bold[white]%}%t )( %{$reset_color%}"
MOVELINE_TIME_="%{$fg[red]%}#%{$fg_bold[white]%}( %{$fg[yellow]%}%W%{$reset_color%}@%{$fg_bold[white]%}%t )( %{$reset_color%}"

# Grab the current machine name
#bold MOVELINE_MACHINE_="%{$fg_bold[blue]%}%m%{$fg[white]%} ):%{$reset_color%}"
MOVELINE_MACHINE_="%{$fg[blue]%}%m%{$fg[white]%} ):%{$reset_color%}"

# Grab the current username
#bold MOVELINE_CURRENT_USER_="%{$fg_bold[white]%}# %{$fg_bold[green]%}%n%{$fg_bold[white]%}. %{$reset_color%}"
MOVELINE_CURRENT_USER_="%{$fg[green]%}%n%{$fg_bold[white]%} %{$reset_color%}"

# Grab the current machine name
#bold MOVELINE_MACHINE_="%{$fg_bold[blue]%}%m%{$fg_bold[white]%}. %{$reset_color%}"
MOVELINE_MACHINE_="on %{$fg[yellow]%}%m%{$fg_bold[white]%} %{$reset_color%}"

# Grab the current filepath, use shortcuts: ~/Desktop
# Append the current git branch and svn branch, if in a git repository: ~aw at master/master
MOVELINE_LOCA_="at %{$fg[cyan]%}%~ \$(git_prompt_info)%{$reset_color%}"
#
# For the git and svn prompt
ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[blue]%}git:(%{$fg[red]%}"

# Close it all off by resetting the color and styles.
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"

# Do nothing if the branch is clean (no changes).
#bold ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg_bold[green]%}✔"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[blue]%}) %{$fg[green]%}✔"

# Add 1 cyan ✗s if this branch is diiirrrty! Dirty branch!
#bold ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg_bold[red]%}✗✗"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[blue]%}) %{$fg[red]%}✗"

# Put it all together!
PROMPT="$MOVELINE_CURRENT_USER_$MOVELINE_MACHINE_$MOVELINE_LOCA_
%{$fg[white]%}Ѳ%{$reset_color%} "
