/*
 * 文 件 名:  HttpPostUtil.java
 * 版    权:  Copyright 2015 咪咕互动娱乐有限公司,  All rights reserved
 * 描    述:  <描述>
 * 版    本： <版本号> 
 * 创 建 人:  wufeng
 * 创建时间:  2016年1月22日
 
 */
package cn.migu.file.core;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.protocol.HTTP;
import org.apache.http.util.EntityUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * 以POST方式发送HTTP请求
 * <功能详细描述>
 * 
 * @author  wufeng
 * @version  [版本号, 2016年1月22日]
 * @see  [相关类/方法]
 * @since  [产品/模块版本]
 */
@SuppressWarnings("deprecation")
public class HttpPostUtil
{
	@SuppressWarnings("unused")
	static private Logger logger = LoggerFactory.getLogger(HttpPostUtil.class);
	
	public static String post(String url, Map<String, String> params) throws Exception
    {
        DefaultHttpClient httpclient = new DefaultHttpClient();
        String body = null;
        
        HttpPost post = postForm(url, params);
        
        body = invoke(httpclient, post);
        
        httpclient.getConnectionManager().shutdown();
        
        return body;
    }
    
	private static String invoke(DefaultHttpClient httpclient, HttpUriRequest httpost) throws Exception
    {
        
        HttpResponse response = sendRequest(httpclient, httpost);
        String body = paseResponse(response);
        
        return body;
    }
    
    private static String paseResponse(HttpResponse response) throws Exception
    {
    	
    	
        HttpEntity entity = response.getEntity();
        
        //        String charset = EntityUtils.getContentCharSet(entity);
        
        String body = null;
        
        body = EntityUtils.toString(entity);
        
       
        
        return body;
    }
    
	private static HttpResponse sendRequest(DefaultHttpClient httpclient, HttpUriRequest httpost)
    {
        HttpResponse response = null;
        
        try
        {
            response = httpclient.execute(httpost);
        }
        catch (ClientProtocolException e)
        {
            e.printStackTrace();
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }
        return response;
    }
    
    private static HttpPost postForm(String url, Map<String, String> params) throws UnsupportedEncodingException
    {
        
        HttpPost httpost = new HttpPost(url);
        List<NameValuePair> nvps = new ArrayList<NameValuePair>();
        
        Set<String> keySet = params.keySet();
        for (String key : keySet)
        {
            nvps.add(new BasicNameValuePair(key, params.get(key)));
        }
        
//        try
//        {
            httpost.setEntity(new UrlEncodedFormEntity(nvps, HTTP.UTF_8));
//        }
//        catch (Exception e)
//        {
//        	logger.warn("##############"+e.getMessage()+"###################");
//            e.printStackTrace();
//        }
        
        return httpost;
    }
}
