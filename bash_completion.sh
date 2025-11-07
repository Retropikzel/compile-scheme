_compile_scheme()
{
    local cur prev opts
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"
    shortopts="-A -I"
    longopts="--list-r6rs --list-r7rs --list-all --help --version"
    targets="unix windows php"
    outputfound=false
    for i in "${COMP_WORDS[@]}"; do
        if [[ "$i" == "-o" ]] ; then
            outputfound=true
        fi
    done
    targetfound=false
    for i in "${COMP_WORDS[@]}"; do
        if [[ "$i" == "-t" ]] ; then
            targetfound=true
        fi
    done

    if [[ "$outputfound" == "false" ]]; then
        shortopts="${shortopts} -o"
    fi
    if [[ "$targetfound" == "false" ]]; then
        shortopts="${shortopts} -t"
    fi

    opts="${shortopts} ${longopts}"

    if [[ ${prev} == -o ]] ; then
        COMPREPLY=( $(compgen -W "" -- ${cur}) )
        return 0
    elif [[ ${prev} == -I ]] ; then
        COMPREPLY=()
        return 0
    elif [[ ${prev} == -A ]] ; then
        COMPREPLY=()
        return 0
    elif [[ ${prev} == -t ]] ; then
        COMPREPLY=( $(compgen -W "${targets}" -- ${cur}) )
        return 0
    elif [[ ${cur} == -o ]] ; then
        COMPREPLY=("")
        return 0
    elif [[ ${cur} == -t ]] ; then
        COMPREPLY=("")
        return 0
    elif [[ ${prev} == *.sps ]] ; then
        COMPREPLY=( $(compgen -W "${shortopts}" -- ${cur}) )
        return 0
    elif [[ ${prev} == *.scm ]] ; then
        COMPREPLY=( $(compgen -W "${shortopts}" -- ${cur}) )
        return 0
    elif [[ ${cur} == -* ]] ; then
        for i in "${COMP_WORDS[@]}"; do
            if [[ "$i" == *.sps ]] ; then
                COMPREPLY=( $(compgen -W "${shortopts}" -- ${cur}) )
                return 0
            fi
            if [[ "$i" == *.scm ]] ; then
                COMPREPLY=( $(compgen -W "${shortopts}" -- ${cur}) )
                return 0
            fi
        done
        COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
        return 0
    elif [[ ${cur} == --* ]] ; then
        for i in "${COMP_WORDS[@]}"; do
            if [[ "$i" == *.sps ]] ; then
                COMPREPLY=( $(compgen -W "${shortopts}" -- ${cur}) )
                return 0
            fi
            if [[ "$i" == *.scm ]] ; then
                COMPREPLY=( $(compgen -W "${shortopts}" -- ${cur}) )
                return 0
            fi
        done
        COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
        return 0
    elif [[ ${cur} == * ]] ; then
        for i in "${COMP_WORDS[@]}"; do
            if [[ "$i" == *.sps ]] ; then
                COMPREPLY=( $(compgen -W "${shortopts}" -- ${cur}) )
                return 0
            fi
            if [[ "$i" == *.scm ]] ; then
                COMPREPLY=( $(compgen -W "${shortopts}" -- ${cur}) )
                return 0
            fi
        done
        COMPREPLY=( $(compgen -W "$(find . -maxdepth 1 -type f -name "*.sps" -o -name "*.scm" -printf '%f\n')" -- ${cur}) )
        return 0
    fi
}
# `foo` <tab> <tab> would show autocomplete above wordlist 
complete -o bashdefault -o default -F _compile_scheme compile-scheme
# # If you want simplest wordlist, use below instead:
#complete -W "--help --verbose --version" compile-scheme
#
