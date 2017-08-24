package com.gus.trace.service;

import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.gus.trace.utils.HttpClientUtils;

@RestController
@RequestMapping("/mydev")
public class HttpService {

	@RequestMapping("/s1")
	public String s1() throws Exception {

		return HttpClientUtils.request4HC("http://127.0.0.1:8081/mydev/s2", null);
	}

	@RequestMapping("/s2")
	public String s2() throws Exception {

		return "ok_s2";
	}
}
