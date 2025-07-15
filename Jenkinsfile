pipeline {
    agent {
        dockerfile {
            filename 'Dockerfile.jenkins'
                args '--user=root -v /var/run/docker.sock:/var/run/docker.sock'
        }
    }
    options {
        disableConcurrentBuilds()
            buildDiscarder(logRotator(numToKeepStr: '10', artifactNumToKeepStr: '10'))
    }
    stages {
        stage('Test R6RS implementations') {
            steps {
                script {
                    def r6rs_implementations = sh(script: 'chibi-scheme -I ./snow -I . compile-r7rs.scm --list-r6rs-schemes', returnStdout: true).split()
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
            steps {
                script {
                    def r7rs_implementations = sh(script: 'chibi-scheme -I ./snow -I . compile-r7rs.scm --list-r7rs-schemes', returnStdout: true).split()
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

    }
}
