package com.gus.service;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;

import redis.clients.jedis.Jedis;
import redis.clients.jedis.JedisPool;
import redis.clients.jedis.Pipeline;

public class LoadTxt2Redis {

	private static JedisPool jedisPool;

	/**
	 * ip port db_index file_name key_name
	 */
	public static void main(String[] args) throws IOException {

		long t1 = System.currentTimeMillis();

		jedisPool = new JedisPool(args[0], Integer.parseInt(args[1]));

		Jedis j = jedisPool.getResource();

		j.select(Integer.parseInt(args[2]));

		Pipeline p = j.pipelined();

		File f = new File(args[3]);

		InputStreamReader read = new InputStreamReader(new FileInputStream(f));
		BufferedReader bufferedReader = new BufferedReader(read);
		String lineTxt = null;
		int i = 0;
		while ((lineTxt = bufferedReader.readLine()) != null) {
			p.sadd(args[4], lineTxt);
			i++;
		}
		read.close();
		p.sync();
		p.close();
		j.close();

		long t2 = (System.currentTimeMillis() - t1);
		System.out.println(
				"key: " + args[4] + " load " + i + " rows to db_index_" + args[2] + " spend " + t2 + " millisec.");
	}
}
