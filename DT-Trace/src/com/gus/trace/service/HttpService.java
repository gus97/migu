package com.gus.trace.service;

import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.gus.trace.utils.HttpClientUtils;

@RestController
@RequestMapping("/mydev")
public class HttpService {

	@RequestMapping("/s1")
	public String s1() throws Exception {

		String rep1 = HttpClientUtils.request4HC("http://127.0.0.1:8081/mydev/s2", null);
		String rep2=  HttpClientUtils.request4HC("http://127.0.0.1:8081/mydev/s3", null);
		
		return rep1+rep2;
		
	}

	@RequestMapping("/s2")
	public String s2() throws Exception {

		return "ok_s2";
	}
	

	@RequestMapping("/s3")
	public String s3() throws Exception {

		String rep1 = HttpClientUtils.request4HC("http://127.0.0.1:8081/mydev/s4", null);
		return "ok_s3"+rep1;
	}
	
	@RequestMapping("/s4")
	public String s4() throws Exception {

		System.out.println(1/0);
		return "ok_s4";
	}
}
