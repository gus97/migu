package com.gus.trace.utils;

import java.io.IOException;
import java.util.List;

import org.apache.http.HttpResponse;
import org.apache.http.client.config.RequestConfig;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;

import com.gus.trace.aop.TraceThreadLocal;

public class HttpClientUtils {

	public static String request4HC(String url, List<BasicNameValuePair> list) {

		CloseableHttpClient httpClient = HttpClients.createDefault();
		RequestConfig requestConfig = RequestConfig.custom().setConnectTimeout(1000).setConnectionRequestTimeout(1000)
				.setSocketTimeout(1000).setRedirectsEnabled(true).build();

		HttpPost httpPost = new HttpPost(url);
		
		httpPost.addHeader("trace-id",TraceThreadLocal.TTL.get().traceID+"");
		httpPost.addHeader("span-id",TraceThreadLocal.TTL.get().spanID+"");
		
		// 设置超时时间
		httpPost.setConfig(requestConfig);

		try {
			if (list != null) {
				UrlEncodedFormEntity entity = new UrlEncodedFormEntity(list, "UTF-8");
				// 设置post求情参数
				httpPost.setEntity(entity);
			}
			HttpResponse httpResponse = httpClient.execute(httpPost);
			String strResult = "";
			if (httpResponse != null) {
				System.out.println(httpResponse.getStatusLine().getStatusCode());
				if (httpResponse.getStatusLine().getStatusCode() == 200) {
					strResult = EntityUtils.toString(httpResponse.getEntity());
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
					httpClient.close(); // 释放资源
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		return null;
	}
}
