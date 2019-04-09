bin/kafka-topics.sh --zookeeper localhost:2181/ --create --topic diameter_queue_10 --partitions 1 --replication-factor 1
bin/kafka-topics.sh --zookeeper localhost:2181/ --create --topic diameter_queue --partitions 1 --replication-factor 1
bin/kafka-topics.sh --zookeeper localhost:2181/ --create --topic dispatcher_fail_Q --partitions 1 --replication-factor 1
bin/kafka-topics.sh --zookeeper localhost:2181/ --create --topic gsm_ingress --partitions 1 --replication-factor 1
bin/kafka-topics.sh --zookeeper localhost:2181/ --create --topic in_dummy --partitions 1 --replication-factor 1
bin/kafka-topics.sh --zookeeper localhost:2181/ --create --topic in_imi --partitions 1 --replication-factor 1
bin/kafka-topics.sh --zookeeper localhost:2181/ --create --topic in_irmtn --partitions 1 --replication-factor 1
bin/kafka-topics.sh --zookeeper localhost:2181/ --create --topic log_event --partitions 1 --replication-factor 1
bin/kafka-topics.sh --zookeeper localhost:2181/ --create --topic radius_input_q --partitions 1 --replication-factor 1
bin/kafka-topics.sh --zookeeper localhost:2181/ --create --topic unit_process_fail_Q --partitions 1 --replication-factor 1
bin/kafka-topics.sh --zookeeper localhost:2181/ --create --topic unit_process_logging_Q --partitions 1 --replication-factor 1
bin/kafka-topics.sh --zookeeper localhost:2181/ --create --topic unit_process_success_Q --partitions 1 --replication-factor 1
bin/kafka-topics.sh --zookeeper localhost:2181/ --create --topic user_process_fail_Q --partitions 1 --replication-factor 1
bin/kafka-topics.sh --zookeeper localhost:2181/ --create --topic user_process_logging_Q --partitions 1 --replication-factor 1
bin/kafka-topics.sh --zookeeper localhost:2181/ --create --topic user_process_success_Q --partitions 1 --replication-factor 1
bin/kafka-topics.sh --zookeeper localhost:2181/ --create --topic user_process_logging_Q --partitions 1 --replication-factor 1














