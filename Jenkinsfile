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

                    r6rs_implementations.each { implementation->
                        stage("${implementation} R6RS") {
                            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
                                sh "make test-r6rs-docker COMPILE_R7RS=${implementation}"
                            }
                        }
                    }
                }
            }
        }

        stage('Test R7RS implementations') {
            steps {
                script {
                    def r7rs_implementations = sh(script: 'chibi-scheme -I ./snow -I . compile-r7rs.scm --list-r7rs-schemes', returnStdout: true).split()

                    r7rs_implementations.each { implementation->
                        stage("${implementation} R7RS") {
                            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
                                sh "timeout 600 make test-r7rs-docker COMPILE_R7RS=${implementation}"
                            }
                        }
                    }
                }
            }
        }

    }
}
