package com.gus.trace.aop;

import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class FilterConfig {

	@Bean
	public FilterRegistrationBean filterRegistration() {
		FilterRegistrationBean registration = new FilterRegistrationBean(new HttpServiceFilter());
		registration.addUrlPatterns("/*");
		return registration;
	}
}
