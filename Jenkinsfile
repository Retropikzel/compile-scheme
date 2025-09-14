pipeline {

    agent {
        label 'docker-x86_64'
    }

    options {
        disableConcurrentBuilds()
        buildDiscarder(logRotator(numToKeepStr: '10', artifactNumToKeepStr: '10'))
    }

    parameters {
        booleanParam(name: 'DOCKER', defaultValue: false, description: 'Build and push docker image')
    }

    stages {
        stage('Test R6RS implementations') {
            steps {
                script {
                    def r6rs_implementations = sh(script: 'docker run retropikzel1/compile-r7rs bash -c "compile-r7rs --list-r6rs-schemes"', returnStdout: true).split()
                    parallel r6rs_implementations.collectEntries { SCHEME ->
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
                    def r7rs_implementations = sh(script: 'docker run retropikzel1/compile-r7rs bash -c "compile-r7rs --list-r7rs-schemes"', returnStdout: true).split()
                    parallel r7rs_implementations.collectEntries { SCHEME ->
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

        stage('Docker build/login/push x84-64') {
            agent {
                label 'linux-x86_64'
            }
            when {
                allOf {
                    branch 'main'
                    expression {
                        return params.DOCKER
                    }
                }
            }
            steps {
                catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
                    sh 'docker build . --tag=retropikzel1/compile-r7rs'
                    sh 'docker login -u ${DOCKER_HUB_USERNAME} -p ${DOCKER_HUB_TOKEN}'
                    sh 'docker push retropikzel1/compile-r7rs'
                }
            }
        }

        stage('Docker build/login/push arm') {
            agent {
                label 'linux-arm'
            }
            when {
                allOf {
                    branch 'main'
                    expression {
                        return params.DOCKER
                    }
                }
            }
            steps {
                catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
                    sh 'docker build . --tag=retropikzel1/compile-r7rs'
                    sh 'docker login -u ${DOCKER_HUB_USERNAME} -p ${DOCKER_HUB_TOKEN}'
                    sh 'docker push retropikzel1/compile-r7rs'
                }
            }
        }

        stage('Docker logout') {
            steps {
                sh 'docker logout'
            }
        }

    }
}
