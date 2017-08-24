package com.gus.service;

import java.lang.management.ManagementFactory;
import java.lang.management.RuntimeMXBean;
import java.lang.reflect.Field;
import java.lang.reflect.Method;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

import io.netty.bootstrap.ServerBootstrap;
import io.netty.channel.ChannelFuture;
import io.netty.channel.ChannelInitializer;
import io.netty.channel.ChannelOption;
import io.netty.channel.EventLoopGroup;
import io.netty.channel.epoll.EpollEventLoopGroup;
import io.netty.channel.epoll.EpollServerSocketChannel;
import io.netty.channel.socket.SocketChannel;
import io.netty.handler.codec.http.HttpRequestDecoder;
import io.netty.handler.codec.http.HttpResponseEncoder;
import redis.clients.jedis.JedisPool;
import redis.clients.jedis.JedisPoolConfig;
import sun.management.VMManagement;

public class PokerService {

	private static Logger log = Logger.getLogger("op");

	private static String REDISDB_IP = "172.18.111.3";
	private static int REDISDB_PORT = 6379;

	public static JedisPool jedisPool;

	static {
		log.info("##################################################################");
		log.info("init redis pool...");
		JedisPoolConfig config = new JedisPoolConfig();
		config.setMaxTotal(200);
		config.setMaxIdle(20);
		config.setMaxWaitMillis(5000L);
		config.setTestOnBorrow(true);
		config.setTestWhileIdle(true);
		config.setTimeBetweenEvictionRunsMillis(60000L);
		jedisPool = new JedisPool(config, REDISDB_IP, REDISDB_PORT);
		log.info("init redis pool successful...");
		log.info("##################################################################");
	}

	public void start(int port) throws Exception {
		//NioEventLoopGroup
		//EpollEventLoopGroup
		EventLoopGroup bossGroup = new EpollEventLoopGroup(); // (1)
		EventLoopGroup workerGroup = new EpollEventLoopGroup();
		try {
			ServerBootstrap b = new ServerBootstrap(); // (2)
			//NioServerSocketChannel
			//EpollServerSocketChannel
			b.group(bossGroup, workerGroup).channel(EpollServerSocketChannel.class) // (3)
					.childHandler(new ChannelInitializer<SocketChannel>() { // (4)
						@Override
						public void initChannel(SocketChannel ch) throws Exception {
							ch.pipeline().addLast(new HttpResponseEncoder());
							ch.pipeline().addLast(new HttpRequestDecoder());
							ch.pipeline().addLast(new PokerServerInboundHandler());
						}
					}).option(ChannelOption.SO_BACKLOG, 128) // (5)
					.childOption(ChannelOption.SO_KEEPALIVE, true); // (6)

			ChannelFuture f = b.bind(port).sync(); // (7)

			f.channel().closeFuture().sync();
		} finally {
			workerGroup.shutdownGracefully();
			bossGroup.shutdownGracefully();
		}
	}

	public static void main(String[] args) throws Exception {
		

		RuntimeMXBean runtime = ManagementFactory.getRuntimeMXBean();
		Field jvm = runtime.getClass().getDeclaredField("jvm");
		jvm.setAccessible(true);
		VMManagement mgmt = (VMManagement) jvm.get(runtime);
		Method pidMethod = mgmt.getClass().getDeclaredMethod("getProcessId");
		pidMethod.setAccessible(true);
		int pid = (Integer) pidMethod.invoke(mgmt);

		log.info("Server PID on " + pid);

		if (args == null || args.length == 0) {

			log.error("Server Port cannot null ");

			return;

		}

		if (!StringUtils.isNumeric(args[0]) || Integer.parseInt(args[0]) > 65535 || Integer.parseInt(args[0]) < 2000) {
			log.error("Server Port on [" + args[0] + "] is invalid");
			return;
		}

		PokerService server = new PokerService();

		log.info("Http Server listening on " + args[0] + " ...");

		server.start(Integer.parseInt(args[0]));

	}
}
