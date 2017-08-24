package com.gus.service;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;

import redis.clients.jedis.Jedis;
import redis.clients.jedis.Pipeline;

public class JedisUtils {

	public static Jedis getJedis() {
		Jedis jedis = PokerService.jedisPool.getResource();
		return jedis;
	}

	public static void releaseJedis(Jedis jedis) {
		if (jedis != null) {
			jedis.close();
		}
	}

	public static void main(String[] args) throws Exception {

		Jedis j = getJedis();
		j.select(1);

		Pipeline p = j.pipelined();

		File f = new File("c:/1/pk/w.txt");
		InputStreamReader read = new InputStreamReader(new FileInputStream(f));
		BufferedReader bufferedReader = new BufferedReader(read);
		String lineTxt = null;
		while ((lineTxt = bufferedReader.readLine()) != null) {
			p.sadd("w", lineTxt);
		}
		read.close();
		p.sync();
		p.close();
		j.close();
	}
}
