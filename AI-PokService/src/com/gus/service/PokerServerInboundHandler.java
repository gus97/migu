package com.gus.service;

import static io.netty.handler.codec.http.HttpResponseStatus.OK;
import static io.netty.handler.codec.http.HttpVersion.HTTP_1_1;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.InetSocketAddress;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

import com.alibaba.fastjson.JSONObject;

import io.netty.buffer.Unpooled;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelInboundHandlerAdapter;
import io.netty.handler.codec.http.DefaultFullHttpResponse;
import io.netty.handler.codec.http.DefaultHttpRequest;
import io.netty.handler.codec.http.FullHttpResponse;
import io.netty.handler.codec.http.HttpHeaderNames;
import io.netty.handler.codec.http.HttpHeaderValues;
import redis.clients.jedis.Jedis;

public class PokerServerInboundHandler extends ChannelInboundHandlerAdapter {

	private static Logger logger = Logger.getLogger("op");

	@Override
	public void channelRead(ChannelHandlerContext ctx, Object msg) throws Exception {

		JSONObject jsonObject = new JSONObject();

		JSONObject jsonObject1 = new JSONObject();

		jsonObject1.put("a_id", "");
		jsonObject1.put("b_id", "");
		jsonObject1.put("c_id", "");
		jsonObject1.put("msg", "request parameter mismatching");

		jsonObject.put("0", jsonObject1);

		String info = "";

		if (msg instanceof DefaultHttpRequest) {

			DefaultHttpRequest request = (DefaultHttpRequest) msg;

			info = request.uri();

			if (!info.equals("/favicon.ico") && info.startsWith("/gp4Lk") && info.indexOf("=") != -1) {
				String ip = request.headers().get("X-Forwarded-For");
				if (ip != null) {
					logger.info("request from ===========> " + ip);
				} else {
					InetSocketAddress insocket = (InetSocketAddress) ctx.channel().remoteAddress();
					logger.info("request from ===========> " + insocket);
				}

				String pa = "";

				String[] s = info.split("=");

				if (s.length == 2) {

					String[] p = s[1].split(",");

					if (s[1].split(",").length != 7 || (!StringUtils.isNumeric(p[2]) || !StringUtils.isNumeric(p[4])
							|| !StringUtils.isNumeric(p[6]))) {

						logger.error("request parameter mismatching " + info);
						FullHttpResponse response = new DefaultFullHttpResponse(HTTP_1_1, OK,
								Unpooled.wrappedBuffer(jsonObject.toJSONString().getBytes()));
						response.headers().set(HttpHeaderNames.CONTENT_TYPE, "text/plain");
						response.headers().set(HttpHeaderNames.CONTENT_LENGTH, response.content().readableBytes());
						response.headers().set(HttpHeaderNames.CONNECTION, HttpHeaderValues.KEEP_ALIVE);
						ctx.write(response);
						ctx.flush();
					} else {

						pa = info.split("=")[1];
						String w = getPoker(pa);

						FullHttpResponse response = new DefaultFullHttpResponse(HTTP_1_1, OK,
								Unpooled.wrappedBuffer(w.getBytes()));
						response.headers().set(HttpHeaderNames.CONTENT_TYPE, "text/plain");
						response.headers().set(HttpHeaderNames.CONTENT_LENGTH, response.content().readableBytes());
						response.headers().set(HttpHeaderNames.CONNECTION, HttpHeaderValues.KEEP_ALIVE);
						ctx.write(response);
						ctx.flush();
					}
				} else {
					logger.error("request parameter mismatching " + info);
					FullHttpResponse response = new DefaultFullHttpResponse(HTTP_1_1, OK,
							Unpooled.wrappedBuffer(jsonObject.toJSONString().getBytes()));
					response.headers().set(HttpHeaderNames.CONTENT_TYPE, "text/plain");
					response.headers().set(HttpHeaderNames.CONTENT_LENGTH, response.content().readableBytes());
					response.headers().set(HttpHeaderNames.CONNECTION, HttpHeaderValues.KEEP_ALIVE);
					ctx.write(response);
					ctx.flush();
				}
			} else if (!info.equals("/favicon.ico") && info.startsWith("/monitor?healthCheck")) {
				logger.info("request healthCheck");
				Jedis j = JedisUtils.getJedis();
				j.select(1);
				String w = j.srandmember("w");
				String b = j.srandmember("b");
				String a = j.srandmember("a");
				
				JedisUtils.releaseJedis(j);
				
				logger.info("pool check ============================>"+PokerService.jedisPool.getNumIdle()+"-----"+PokerService.jedisPool.getNumActive()+"-----"+PokerService.jedisPool.getNumWaiters());
				

				if (w != null && b != null && a != null) {
					logger.info("request healthCheck ---> a " + a);
					logger.info("request healthCheck ---> b " + b);
					logger.info("request healthCheck ---> w " + w);

					FullHttpResponse response = new DefaultFullHttpResponse(HTTP_1_1, OK,
							Unpooled.wrappedBuffer("ok".getBytes()));
					response.headers().set(HttpHeaderNames.CONTENT_TYPE, "text/plain");
					response.headers().set(HttpHeaderNames.CONTENT_LENGTH, response.content().readableBytes());
					response.headers().set(HttpHeaderNames.CONNECTION, HttpHeaderValues.KEEP_ALIVE);
					ctx.write(response);
					ctx.flush();
				} else {
					FullHttpResponse response = new DefaultFullHttpResponse(HTTP_1_1, OK,
							Unpooled.wrappedBuffer("fail".getBytes()));
					response.headers().set(HttpHeaderNames.CONTENT_TYPE, "text/plain");
					response.headers().set(HttpHeaderNames.CONTENT_LENGTH, response.content().readableBytes());
					response.headers().set(HttpHeaderNames.CONNECTION, HttpHeaderValues.KEEP_ALIVE);
					ctx.write(response);
					ctx.flush();
				}
				
				
				
			} else if (!info.startsWith("/gp4Lk") || info.indexOf("=") == -1) {

				if (!info.startsWith("/favicon.ico")) {

					logger.error("request parameter mismatching" + info);
					FullHttpResponse response = new DefaultFullHttpResponse(HTTP_1_1, OK,
							Unpooled.wrappedBuffer(jsonObject.toJSONString().getBytes()));
					response.headers().set(HttpHeaderNames.CONTENT_TYPE, "text/plain");
					response.headers().set(HttpHeaderNames.CONTENT_LENGTH, response.content().readableBytes());
					response.headers().set(HttpHeaderNames.CONNECTION, HttpHeaderValues.KEEP_ALIVE);
					ctx.write(response);
					ctx.flush();
				}
			}
		}

	}

	@Override
	public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) throws Exception {

		StringWriter stringWriter = new StringWriter();
		PrintWriter writer = new PrintWriter(stringWriter);
		cause.printStackTrace(writer);
		StringBuffer buffer = stringWriter.getBuffer();
		logger.error(buffer.toString());

		FullHttpResponse response = new DefaultFullHttpResponse(HTTP_1_1, OK,
				Unpooled.wrappedBuffer(("500\n" + cause.getMessage()).getBytes()));
		response.headers().set(HttpHeaderNames.CONTENT_TYPE, "text/plain");
		response.headers().set(HttpHeaderNames.CONTENT_LENGTH, response.content().readableBytes());
		response.headers().set(HttpHeaderNames.CONNECTION, HttpHeaderValues.KEEP_ALIVE);
		ctx.write(response);
		ctx.flush();
	}

	@Override
	public void channelReadComplete(ChannelHandlerContext ctx) throws Exception {

		ctx.flush();
	}

	public static String getPoker(String parm) {

		String[] p = parm.split(",");

		String sNum = p[0];

		int pa = Integer.parseInt(p[2]);
		int pb = Integer.parseInt(p[4]);
		int pc = Integer.parseInt(p[6]);

		Map<String, Integer> map = new LinkedHashMap<>();
		map.put(p[1], pa);
		map.put(p[3], pb);
		map.put(p[5], pc);

		Map<String, Integer> result = new LinkedHashMap<>();

		map.entrySet().stream().sorted(Map.Entry.<String, Integer>comparingByValue().reversed())
				.forEachOrdered(x -> result.put(x.getKey(), x.getValue()));

		/**
		 * 如果Na-Nb<2，发一套平衡牌； 否则，如果Na-Nc>=8且Na-Nb<8，发一套必胜牌给A
		 * 否则，如果Na-Nb<4，发一套优势牌给A 否则，如果Na-Nb<8，发一套必胜牌给A 否则，发一套必胜牌给A
		 */

		String sida = "";
		String sidb = "";
		String sidc = "";

		Integer nla = 0;
		Integer nlb = 0;
		Integer nlc = 0;

		int i = 0;
		for (String s : result.keySet()) {
			if (i == 0) {
				sida = s;
				nla = result.get(s);
			} else if (i == 1) {
				sidb = s;
				nlb = result.get(s);
			} else if (i == 2) {
				sidc = s;
				nlc = result.get(s);
			}
			i++;
		}

		Jedis j = JedisUtils.getJedis();
		j.select(1);

		JSONObject jsonObject = new JSONObject();
		JSONObject jsonObjectChild = new JSONObject();

		Set<String> s = new HashSet<String>();
		// 平衡牌
		if (nla - nlb < 2) {
			String r = j.srandmember("b");
			jsonObjectChild.put(sida, Convert.convert(r.split("\\$")[0]));
			jsonObjectChild.put(sidb, Convert.convert(r.split("\\$")[1]));
			jsonObjectChild.put(sidc, Convert.convert(r.split("\\$")[2]));
			jsonObjectChild.put("three", Convert.convert(r.split("\\$")[3]));
			jsonObject.put(sNum, jsonObjectChild);
			logger.info(parm + "------B------>" + r);
		}
		// 必胜牌
		else if (nla - nlc >= 8 && nla - nlb < 8) {
			String r = j.srandmember("w");
			jsonObjectChild.put(sida, Convert.convert(r.split("\\$")[0]));
			jsonObjectChild.put(sidb, Convert.convert(r.split("\\$")[1]));
			jsonObjectChild.put(sidc, Convert.convert(r.split("\\$")[2]));
			jsonObjectChild.put("three", Convert.convert(r.split("\\$")[3]));
			jsonObject.put(sNum, jsonObjectChild);
			logger.info(parm + "------W------>" + r);
		}
		// 优势牌
		else if (nla - nlb < 4) {
			String r = j.srandmember("a");
			jsonObjectChild.put(sida, Convert.convert(r.split("\\$")[0]));
			jsonObjectChild.put(sidb, Convert.convert(r.split("\\$")[1]));
			jsonObjectChild.put(sidc, Convert.convert(r.split("\\$")[2]));
			jsonObjectChild.put("three", Convert.convert(r.split("\\$")[3]));
			jsonObject.put(sNum, jsonObjectChild);
			logger.info(parm + "------A------>" + r);
		} else if (nla - nlb < 8) {
			String r = j.srandmember("w");
			jsonObjectChild.put(sida, Convert.convert(r.split("\\$")[0]));
			jsonObjectChild.put(sidb, Convert.convert(r.split("\\$")[1]));
			jsonObjectChild.put(sidc, Convert.convert(r.split("\\$")[2]));
			jsonObjectChild.put("three", Convert.convert(r.split("\\$")[3]));
			jsonObject.put(sNum, jsonObjectChild);
			logger.info(parm + "------W------>" + r);
		} else {
			String r = j.srandmember("w");
			jsonObjectChild.put(sida, Convert.convert(r.split("\\$")[0]));
			jsonObjectChild.put(sidb, Convert.convert(r.split("\\$")[1]));
			jsonObjectChild.put(sidc, Convert.convert(r.split("\\$")[2]));
			jsonObjectChild.put("three", Convert.convert(r.split("\\$")[3]));
			jsonObject.put(sNum, jsonObjectChild);
			logger.info(parm + "------W------>" + r);
		}
		JedisUtils.releaseJedis(j);

		for (String s1 : jsonObject.getJSONObject(sNum).get(sida).toString().split(",")) {
			s.add(s1);
		}

		for (String s1 : jsonObject.getJSONObject(sNum).get(sidb).toString().split(",")) {
			s.add(s1);
		}

		for (String s1 : jsonObject.getJSONObject(sNum).get(sidc).toString().split(",")) {
			s.add(s1);
		}

		for (String s1 : jsonObject.getJSONObject(sNum).get("three").toString().split(",")) {
			s.add(s1);
		}

		logger.info(sNum + "---" + s.size());

		return jsonObject.toJSONString();
	}
}
