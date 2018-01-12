#spark-2.2.1

~~~scala
 val appName = "Spark-Streaming-Kafka-" + System.currentTimeMillis()
 val spark = SparkSession.builder()
      .appName(appName)
      .master("spark://172.18.111.3:7078")
      .enableHiveSupport()
      .config("spark.executor.memory", "512m")
      .config("spark.driver.cores", 1)
      .config("spark.cores.max", 4)
      .config("spark.driver.memory", "512m")
      .config("spark.driver.host", "172.16.39.65")
      .config("spark.eventLog.dir", "hdfs://172.18.111.3:9000/spark-2.2.1/applicationHistory")
      .config("spark.eventLog.enabled", value = true)
      .config("spark.eventLog.compress", value = true)
      
       /**
        * ====================
        * 为Streaming做特别优化
        * ====================
        */
      //Spark 将 gracefully （缓慢地）关闭在 JVM 运行的 StreamingContext ,尽可能保证在宕机时不丢数据.
      .config("spark.streaming.stopGracefullyOnShutdown", true)

      //spark自动根据系统负载选择最优消费速率
      .config("spark.streaming.backpressure.enabled", true)

      //冷启动时队列里面有大量积压，防止第一次全部读取，造成系统阻塞，仅仅第一次，后面由spark内部负载自动接管
      .config("spark.streaming.backpressure.initialRate", 10000)

      //限制每秒每个消费线程读取每个kafka分区最大的数据量,若系统自动负载，1.6当初使用，建议不用
      //.config("spark.streaming.kafka.maxRatePerPartition",???)
      .getOrCreate()

    spark.sparkContext.addJar("hdfs://172.18.111.3:9000/gjx/ss.jar")

    spark.sql("select * from gus.emp").show


    spark.sparkContext.setLogLevel("WARN")

    val streamingContext = new StreamingContext(spark.sparkContext, Seconds(5))

    val kafkaParams = Map[String, Object](
      "bootstrap.servers" -> "172.18.111.7:9092,172.18.111.8:9092,172.18.111.9:9092",
      "key.deserializer" -> classOf[StringDeserializer],
      "value.deserializer" -> classOf[StringDeserializer],
      "group.id" -> "gus01",
      "auto.offset.reset" -> "latest",
      "enable.auto.commit" -> (false: java.lang.Boolean))

    val topics = Array("gxc200")


    /**
      * 1、注册各种外部缓存表，通过读取配置
      */

    /**
      * 2、当前流的临时结果表
      * spark.sql("create table if not exists boo (dno string,ename string)")
      */

    val stream = KafkaUtils.createDirectStream[String, String](
      streamingContext,
      PreferConsistent,
      Subscribe[String, String](topics, kafkaParams))


    val schemaString = "dno,ename"

    val fields = new util.ArrayList[StructField]
    for (fieldName <- schemaString.split(",")) {
      val field = DataTypes.createStructField(fieldName, DataTypes.StringType, true)
      fields.add(field)
    }
    val schema = DataTypes.createStructType(fields)

    stream.foreachRDD { rdd =>

      val offsetRanges = rdd.asInstanceOf[HasOffsetRanges].offsetRanges

      rdd.foreachPartition { _ =>

        val o: OffsetRange = offsetRanges(TaskContext.get.partitionId)

        println(s"${o.topic} ${o.partition} ${o.fromOffset} ${o.untilOffset}====================>>>")
      }

      //第一层先清洗
      /**
        * dityRDD = rdd.filter(x=>!x...)
        *  dityRDD.write(hive,mysql,oracle,redis)
        */

      //第二层清洗出干净数据
      val sRDD = rdd.map(_.value().split(",")).filter(_.length == 2)
        .map { x =>
          var seq = mutable.Seq[String]()
          for (i <- 0 until 2) {
            seq = seq :+ x(i)
          }
          //根据正则过滤的数据进入mysql---->>>
          Row(seq: _*)
        }

      //源数据先注册临时表
      spark.createDataFrame(sRDD, schema).createOrReplaceTempView("foo")

      //因为不是structured，按之2.X之前的套路，这里计算好的的数据，先写入临时表

      //具体业务链，抽象，根据不同的输入写入不同的表
      spark.sql("insert into gus.boo select d.name,f.ename from foo f,gus.dept d where d.id = f.dno")

      //业务执行完成回写spark offset
      stream.asInstanceOf[CanCommitOffsets].commitAsync(offsetRanges)
    }

    streamingContext.start()
    streamingContext.awaitTermination()
~~~
