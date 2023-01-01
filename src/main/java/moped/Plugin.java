//
// Moped - a scalable editor extensible via JVM languages
// http://github.com/moped/moped/blob/master/LICENSE

package moped;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Marks a class as implementing a Moped service plugin. Service plugins are arbitrary classes
 * (implementing a common interface) that allow a service to be extended by other packages.
 * Concrete service implementations must have this annotation.
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface Plugin {}
