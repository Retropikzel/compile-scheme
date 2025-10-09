pipeline {

    agent {
        docker {
            label 'docker-x86_64'
            image 'schemers/chibi:head'
            args '--user=root --privileged -v /var/run/docker.sock:/var/run/docker.sock'
        }
    }

    options {
        disableConcurrentBuilds()
        buildDiscarder(logRotator(numToKeepStr: '10', artifactNumToKeepStr: '10'))
    }

    stages {
        stage('Build and install') {
            steps {
                sh "apt-get update && apt-get install -y make libffi-dev build-essential"
                sh 'snow-chibi install --always-yes "(foreign c)"'
                sh 'snow-chibi install --always-yes "(srfi 170)"'
                sh "make build-chibi"
                sh "make install"
            }
        }

        /*
        stage('Test R6RS implementations') {
            steps {
                script {
                    def r6rs_implementations = "chezscheme guile ikarus ironscheme larceny loko mosh racket sagittarius ypsilon".split()
                    r6rs_implementations.collectEntries { SCHEME ->
                        [(SCHEME): {
                                stage("${SCHEME} R6RS") {
                                    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
                                        def DOCKERIMG="${SCHEME}:head"
                                        if("${SCHEME}" == "chicken") {
                                            DOCKERIMG="chicken:5"
                                        }
                                        sh "docker build -f Dockerfile.test --build-arg IMAGE=${DOCKERIMG} --build-arg SCHEME=${SCHEME} --tag=compile-r7rs-test-${SCHEME} ."
                                        sh "docker run -v ${WORKSPACE}:/workdir -w /workdir -t compile-r7rs-test-${SCHEME} sh -c \"make && make install && make SCHEME=${SCHEME} test-r6rs\""
                                    }
                                }
                            }
                        ]
                    }
                }
            }
        }

        stage('Test R7RS implementations') {
            steps {
                script {
                    def r7rs_implementations = "chibi chicken cyclone gambit foment gauche guile kawa larceny loko meevax mit-scheme mosh racket sagittarius skint stklos tr7 ypsilon".split()
                    r7rs_implementations.collectEntries { SCHEME ->
                        [(SCHEME): {
                                stage("${SCHEME} R7RS") {
                                    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
                                        def DOCKERIMG="${SCHEME}:head"
                                        if("${SCHEME}" == "chicken") {
                                            DOCKERIMG="chicken:5"
                                        }
                                        sh "docker build -f Dockerfile.test --build-arg IMAGE=${DOCKERIMG} --build-arg SCHEME=${SCHEME} --tag=compile-r7rs-test-${SCHEME} ."
                                        sh "docker run -v ${WORKSPACE}:/workdir -w /workdir -t compile-r7rs-test-${SCHEME} sh -c \"make && make install && make SCHEME=${SCHEME} test-r7rs\""
                                    }
                                }
                            }
                        ]
                    }
                }
            }
        }
        */
    }
}
