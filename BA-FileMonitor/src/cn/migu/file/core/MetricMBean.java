package cn.migu.file.core;

import java.util.Iterator;
import java.util.Map;

import javax.management.Attribute;
import javax.management.AttributeList;
import javax.management.AttributeNotFoundException;
import javax.management.DynamicMBean;
import javax.management.InvalidAttributeValueException;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanException;
import javax.management.MBeanInfo;
import javax.management.MBeanOperationInfo;
import javax.management.ReflectionException;

public class MetricMBean implements DynamicMBean{
	
	private Map<String, MetricInfo> metric;
	
	public Map<String, MetricInfo> getMetric() {
		return metric;
	}

	public void setMetric(Map<String, MetricInfo> metric) {
		this.metric = metric;
	}
	

	@Override
	public Object getAttribute(String attribute)
			throws AttributeNotFoundException, MBeanException, ReflectionException {
		if (attribute == null)
        {
            throw new AttributeNotFoundException();
        }
        if ("metric".equalsIgnoreCase(attribute))
        {
            return getMetric();
        }
        throw new AttributeNotFoundException();
	}

	@Override
	public void setAttribute(Attribute attribute)
			throws AttributeNotFoundException, InvalidAttributeValueException, MBeanException, ReflectionException {
		String name = attribute.getName();
        @SuppressWarnings("unchecked")
		Map<String, MetricInfo> value = (Map<String, MetricInfo>)attribute.getValue();
        if ("metric".equalsIgnoreCase(name))
        {
            this.setMetric(value);
            return;
        }
        throw new AttributeNotFoundException();
		
	}

	@Override
	public AttributeList getAttributes(String[] attributeNames) {
		if (attributeNames == null)
        {
            return null;
        }
        
        AttributeList resultList = new AttributeList();
        if (attributeNames.length == 0)
        {
            return resultList;
        }
        
        for (int i = 0; i < attributeNames.length; i++)
        {
            try
            {
                Object value = getAttribute(attributeNames[i]);
                resultList.add(new Attribute(attributeNames[i], value));
            }
            catch (Exception e)
            {
                e.printStackTrace();
            }
        }
        return resultList;
	}

	@SuppressWarnings("rawtypes")
	@Override
	public AttributeList setAttributes(AttributeList attributes) {
		if (attributes == null)
        {
            return null;
        }
        
        AttributeList resultList = new AttributeList();
        if (attributes.isEmpty())
        {
            return resultList;
        }
        
        for (Iterator i = attributes.iterator(); i.hasNext();)
        {
            Attribute attr = (Attribute)i.next();
            try
            {
                setAttribute(attr);
                String name = attr.getName();
                Object value = getAttribute(name);
                resultList.add(new Attribute(name, value));
            }
            catch (Exception e)
            {
                e.printStackTrace();
            }
        }
        return resultList;
	}

	@Override
	public Object invoke(String actionName, Object[] params, String[] signature)
			throws MBeanException, ReflectionException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public MBeanInfo getMBeanInfo() {
		MBeanAttributeInfo[] dAttributes =
	            new MBeanAttributeInfo[] {new MBeanAttributeInfo("metric", "java.util.Map", "migu-JMX 监控", true, true, false)};
	        MBeanOperationInfo opers[] = null;
	        
	        MBeanInfo info = new MBeanInfo(this.getClass().getName(), "metricDynamic", dAttributes, null, opers, null);
	        return info;
	}
	
	public interface MetricConstants{
		
		/**最后更新时间*/
		String LASTUPDATETIME ="lastUpdateTime";
		
		/**时间间隔 以 <b>秒</b> 为单位*/
		String INTERVAL ="interval";
	}

}
