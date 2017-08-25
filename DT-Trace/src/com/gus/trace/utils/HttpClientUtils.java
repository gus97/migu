package com.gus.trace.utils;

import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.List;

import org.apache.http.HttpResponse;
import org.apache.http.client.config.RequestConfig;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.gus.trace.aop.TraceInfo;
import com.gus.trace.aop.TraceThreadLocal;

public class HttpClientUtils {

	private final static Logger logger = LoggerFactory.getLogger(HttpClientUtils.class);
	
	public static String request4HC(String url, List<BasicNameValuePair> list) throws UnknownHostException {


		TraceInfo before;

		TraceInfo after;
		
		String ip = InetAddress.getLocalHost().getHostAddress();
		
		CloseableHttpClient httpClient = HttpClients.createDefault();
		RequestConfig requestConfig = RequestConfig.custom().setConnectTimeout(1000).setConnectionRequestTimeout(1000)
				.setSocketTimeout(1000).setRedirectsEnabled(true).build();

		HttpPost httpPost = new HttpPost(url);
		
		httpPost.addHeader("trace-id",TraceThreadLocal.TTL.get().traceID+"");
		httpPost.addHeader("span-id",TraceThreadLocal.TTL.get().spanID+"");

		httpPost.setConfig(requestConfig);

		try {
			if (list != null) {
				UrlEncodedFormEntity entity = new UrlEncodedFormEntity(list, "UTF-8");
				
				httpPost.setEntity(entity);
			}
			//before
			before = new TraceInfo(TraceThreadLocal.TTL.get().traceID, TraceThreadLocal.TTL.get().spanID, TraceThreadLocal.TTL.get().parentID,
					TraceThreadLocal.TTL.get().url, "CS", System.currentTimeMillis(),ip,null);
			
			logger.info(before.toString());
			
			HttpResponse httpResponse = httpClient.execute(httpPost);
			String strResult = "";
			if (httpResponse != null) {
				
				if (httpResponse.getStatusLine().getStatusCode() == 200) {
					strResult = EntityUtils.toString(httpResponse.getEntity());
					
					//after
					after = new TraceInfo(TraceThreadLocal.TTL.get().traceID, TraceThreadLocal.TTL.get().spanID, TraceThreadLocal.TTL.get().parentID,
							TraceThreadLocal.TTL.get().url, "CR", System.currentTimeMillis(),ip,null);
					logger.info(after.toString());
					
				} else if (httpResponse.getStatusLine().getStatusCode() == 400) {
					strResult = "Error Response: " + httpResponse.getStatusLine().toString();
				} else if (httpResponse.getStatusLine().getStatusCode() == 500) {
					strResult = "Error Response: " + httpResponse.getStatusLine().toString();
				} else {
					strResult = "Error Response: " + httpResponse.getStatusLine().toString();
				}
			}
			return strResult;
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			try {
				if (httpClient != null) {
					httpClient.close(); 
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		return null;
	}
}
