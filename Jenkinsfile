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
            agent {
                dockerfile {
                    label 'docker-x86_64'
                    filename 'Dockerfile.jenkins'
                    args '--user=root -v /var/run/docker.sock:/var/run/docker.sock'
                }
            }
            steps {
                script {
                    def r6rs_implementations = sh(script: 'chibi-scheme compile-r7rs.scm --list-r6rs-schemes', returnStdout: true).split()
                    parallel r6rs_implementations.collectEntries { implementation->
                        [(implementation): {
                                stage("${implementation} R6RS") {
                                    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
                                        sh "make test-r6rs-docker SCHEME=${implementation}"
                                    }
                                }
                            }
                        ]
                    }
                }
            }
        }

        stage('Test R7RS implementations') {
            agent {
                dockerfile {
                    label 'docker-x86_64'
                    filename 'Dockerfile.jenkins'
                    args '--user=root -v /var/run/docker.sock:/var/run/docker.sock'
                }
            }
            steps {
                script {
                    def r7rs_implementations = sh(script: 'chibi-scheme compile-r7rs.scm --list-r7rs-schemes', returnStdout: true).split()
                    parallel r7rs_implementations.collectEntries { implementation->
                        [(implementation): {
                                stage("${implementation} R7RS") {
                                    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
                                        sh "make test-r7rs-docker SCHEME=${implementation}"
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
